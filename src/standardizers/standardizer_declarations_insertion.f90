module standardizer_declarations_insertion
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_implicit_statement
    use ast_nodes_core, only: literal_node, program_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use ast_nodes_misc, only: blank_line_node, comment_node, directive_node, &
                              implicit_statement_node, intrinsic_statement_node, &
                              use_statement_node, allocate_statement_node, &
                              namelist_statement_node
    use ast_nodes_procedure, only: function_def_node
    use ast_base, only: LITERAL_STRING
    use lexer_core, only: to_lower
    use standardizer_declarations_collection, only: collect_statement_vars
    use standardizer_declarations_parsing, only: apply_type_string_to_decl, &
                                                 update_existing_declaration_type
    use standardizer_declarations_state, only: get_standardizer_type_standardization
    use uid_generator, only: generate_uid
    implicit none
    private

    public :: insert_variable_declarations
    public :: has_implicit_none
    public :: program_has_variable_declarations
    public :: find_declaration_insertion_point
    public :: find_declaration_header_end
    public :: generate_and_insert_declarations
    public :: standardize_declarations
    public :: create_single_declaration
    public :: has_explicit_declaration

contains

    subroutine insert_variable_declarations(arena, prog, prog_index)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: prog_index
        integer, allocatable :: new_body_indices(:)
        integer :: implicit_none_index
        integer, allocatable :: declaration_indices(:)
        integer :: i, j, implicit_insert_pos, header_insert_pos
        integer :: n_declarations, total_extra
        integer :: header_copy_end, separator_start

        if (.not. allocated(prog%body_indices)) return

        implicit_insert_pos = find_declaration_insertion_point(arena, prog)
        if (implicit_insert_pos == 0) then
            implicit_insert_pos = 1
        end if
        header_insert_pos = find_declaration_header_end(arena, prog)
        if (header_insert_pos < implicit_insert_pos) then
            header_insert_pos = implicit_insert_pos
        end if

        if (.not. has_implicit_none(arena, prog)) then
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                          line=1, column=1, &
                                                          parent_index=prog_index)
        else
            implicit_none_index = 0
        end if

        call generate_and_insert_declarations(arena, prog, prog_index, &
                                              declaration_indices)
        n_declarations = 0
        if (allocated(declaration_indices)) n_declarations = size(declaration_indices)

        total_extra = n_declarations
        if (implicit_none_index > 0) total_extra = total_extra + 1
        allocate (new_body_indices(size(prog%body_indices) + total_extra))

        j = 1
        if (implicit_insert_pos > 1) then
            do i = 1, implicit_insert_pos - 1
                new_body_indices(j) = prog%body_indices(i)
                j = j + 1
            end do
        end if

        if (implicit_none_index > 0) then
            new_body_indices(j) = implicit_none_index
            j = j + 1
        end if

        header_copy_end = header_insert_pos - 1
        if (header_copy_end >= implicit_insert_pos) then
            do while (header_copy_end >= implicit_insert_pos)
                if (.not. is_header_separator(arena, prog, header_copy_end)) exit
                header_copy_end = header_copy_end - 1
            end do
        else
            header_copy_end = implicit_insert_pos - 1
        end if
        separator_start = header_copy_end + 1
        if (separator_start < implicit_insert_pos) separator_start = &
            implicit_insert_pos

        if (header_copy_end >= implicit_insert_pos) then
            do i = implicit_insert_pos, header_copy_end
                new_body_indices(j) = prog%body_indices(i)
                j = j + 1
            end do
        end if

        do i = 1, n_declarations
            new_body_indices(j) = declaration_indices(i)
            j = j + 1
        end do

        if (separator_start <= header_insert_pos - 1) then
            do i = separator_start, header_insert_pos - 1
                new_body_indices(j) = prog%body_indices(i)
                j = j + 1
            end do
        end if

        if (header_insert_pos <= size(prog%body_indices)) then
            do i = header_insert_pos, size(prog%body_indices)
                new_body_indices(j) = prog%body_indices(i)
                j = j + 1
            end do
        end if

        prog%body_indices = new_body_indices
        arena%entries(prog_index)%node = prog
    end subroutine insert_variable_declarations

    logical function has_implicit_none(arena, prog) result(found)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: i

        found = .false.
        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (literal_node)
                        if (stmt%literal_kind == LITERAL_STRING .and. &
                            index(stmt%value, "implicit none") > 0) then
                            found = .true.
                            return
                        end if
                    type is (implicit_statement_node)
                        found = .true.
                        return
                    end select
                end if
            end if
        end do
    end function has_implicit_none

    logical function program_has_variable_declarations(arena, prog)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: i

        program_has_variable_declarations = .false.
        if (.not. allocated(prog%body_indices)) return
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (declaration_node)
                        program_has_variable_declarations = .true.
                        return
                    type is (parameter_declaration_node)
                        program_has_variable_declarations = .true.
                        return
                    end select
                end if
            end if
        end do
    end function program_has_variable_declarations

    logical function is_legacy_statement_text(text)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lowered_text

        is_legacy_statement_text = .false.
        if (len(text) == 0) return

        lowered_text = to_lower(adjustl(trim(text)))
        if (len_trim(lowered_text) >= 11) then
            if (index(lowered_text, "equivalence") == 1) then
                is_legacy_statement_text = .true.
                return
            end if
        end if
        if (len_trim(lowered_text) >= 6) then
            if (index(lowered_text, "common") == 1) then
                is_legacy_statement_text = .true.
                return
            end if
        end if
        if (len_trim(lowered_text) >= 5) then
            if (index(lowered_text, "block") == 1) then
                is_legacy_statement_text = .true.
                return
            end if
        end if
    end function is_legacy_statement_text

    logical function is_header_separator(arena, prog, pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer, intent(in) :: pos
        integer :: node_index
        character(len=:), allocatable :: comment_text

        is_header_separator = .false.
        if (.not. allocated(prog%body_indices)) return
        if (pos < 1 .or. pos > size(prog%body_indices)) return

        node_index = prog%body_indices(pos)
        if (.not. arena%has_node_at(node_index)) return

        select type (stmt => arena%entries(node_index)%node)
        type is (comment_node)
            if (allocated(stmt%text)) then
                comment_text = stmt%text
            else
                comment_text = ""
            end if
            is_header_separator = .not. is_legacy_statement_text(comment_text)
        type is (directive_node)
            if (allocated(stmt%text)) then
                comment_text = stmt%text
            else
                comment_text = ""
            end if
            is_header_separator = .not. is_legacy_statement_text(comment_text)
        type is (blank_line_node)
            is_header_separator = .true.
        class default
            is_header_separator = .false.
        end select
    end function is_header_separator

    integer function find_prefix_end(arena, prog, mode) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer, intent(in) :: mode
        integer :: i
        logical :: keep_scanning
        character(len=:), allocatable :: comment_text

        pos = 1
        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    keep_scanning = .false.
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (use_statement_node)
                        keep_scanning = .true.
                    type is (intrinsic_statement_node)
                        keep_scanning = .true.
                    type is (comment_node)
                        if (allocated(stmt%text)) then
                            comment_text = stmt%text
                        else
                            comment_text = ""
                        end if
                        if (is_legacy_statement_text(comment_text)) then
                            keep_scanning = (mode >= 2)
                        else
                            keep_scanning = (mode >= 1)
                        end if
                    type is (directive_node)
                        if (allocated(stmt%text)) then
                            comment_text = stmt%text
                        else
                            comment_text = ""
                        end if
                        if (is_legacy_statement_text(comment_text)) then
                            keep_scanning = (mode >= 2)
                        else
                            keep_scanning = (mode >= 1)
                        end if
                    type is (blank_line_node)
                        keep_scanning = (mode >= 1)
                    type is (implicit_statement_node)
                        keep_scanning = (mode >= 2)
                    type is (declaration_node)
                        keep_scanning = (mode >= 2)
                    type is (parameter_declaration_node)
                        keep_scanning = (mode >= 2)
                    type is (namelist_statement_node)
                        keep_scanning = (mode >= 2)
                    class default
                        keep_scanning = .false.
                    end select

                    if (keep_scanning) then
                        pos = i + 1
                    else
                        exit
                    end if
                end if
            end if
        end do
    end function find_prefix_end

    function find_declaration_insertion_point(arena, prog) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: pos

        pos = find_prefix_end(arena, prog, 1)
    end function find_declaration_insertion_point

    function find_declaration_header_end(arena, prog) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: pos

        pos = find_prefix_end(arena, prog, 2)
    end function find_declaration_header_end

    subroutine standardize_declarations(arena, prog)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer :: i
        logical :: type_standardization_enabled

        if (.not. allocated(prog%body_indices)) return

        call get_standardizer_type_standardization(type_standardization_enabled)

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (declaration_node)
                        if (stmt%type_name == "real" .and. &
                            type_standardization_enabled) then
                            stmt%type_name = "real"
                            stmt%has_kind = .true.
                            stmt%kind_value = 8
                        end if
                        arena%entries(prog%body_indices(i))%node = stmt
                    end select
                end if
            end if
        end do
    end subroutine standardize_declarations

    subroutine generate_and_insert_declarations(arena, prog, prog_index, &
                                                declaration_indices)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer, intent(in) :: prog_index
        integer, allocatable, intent(out) :: declaration_indices(:)
        character(len=64), allocatable :: var_names(:)
        character(len=64), allocatable :: var_types(:)
        logical, allocatable :: var_declared(:)
        character(len=64), allocatable :: function_names(:)
        integer :: i, var_count, func_count
        type(declaration_node) :: decl_node

        allocate (var_names(100))
        allocate (var_types(100))
        allocate (var_declared(100))
        allocate (function_names(100))
        var_names = ''
        var_types = ''
        var_declared = .false.
        function_names = ''
        var_count = 0
        func_count = 0

        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (function_def_node)
                            if (func_count < size(function_names)) then
                                func_count = func_count + 1
                                function_names(func_count) = to_lower(trim(stmt%name))
                            end if
                        end select
                    end if
                end if
            end do
        end if

        ! First pass: process explicit declarations and allocate statements
        ! before processing assignments (fixes #2069)
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (declaration_node)
                            call collect_statement_vars(arena, prog%body_indices(i), &
                                                        var_names, var_types, &
                                                        var_declared, var_count, &
                                                        function_names, func_count)
                        type is (allocate_statement_node)
                            call collect_statement_vars(arena, prog%body_indices(i), &
                                                        var_names, var_types, &
                                                        var_declared, var_count, &
                                                        function_names, func_count)
                        end select
                    end if
                end if
            end do
        end if

        ! Second pass: process all other statements after declarations and allocates
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (declaration_node)
                            ! Skip: already processed in first pass
                        type is (allocate_statement_node)
                            ! Skip: already processed in first pass
                        class default
                            call collect_statement_vars(arena, prog%body_indices(i), &
                                                        var_names, var_types, &
                                                        var_declared, var_count, &
                                                        function_names, func_count)
                        end select
                    end if
                end if
            end do
        end if

        if (var_count > 0) then
            if (allocated(prog%body_indices)) then
                do i = 1, var_count
                    if (len_trim(var_names(i)) == 0) cycle
                    if (len_trim(var_types(i)) == 0) cycle
                    ! If variable is already explicitly declared, mark it as not needing
                    ! a generated declaration (fixes nested implied-do duplicate issue)
                    if (var_declared(i) .and. &
                        has_explicit_declaration(arena, prog, var_names(i))) then
                        var_declared(i) = .false.
                    end if
                    if (has_explicit_declaration(arena, prog, var_names(i))) then
                        block
                            character(len=:), allocatable :: lowered_type
                            lowered_type = to_lower(var_types(i))
                            if (index(lowered_type, 'dimension(') > 0) then
                                call update_existing_declaration_type(arena, &
                                                                      prog_index, &
                                                                      var_names(i), &
                                                                      var_types(i))
                            end if
                        end block
                    end if
                end do
            end if
        end if

        call create_declaration_nodes(arena, prog, prog_index, var_names, &
                                      var_types, var_declared, var_count, &
                                      declaration_indices)
    end subroutine generate_and_insert_declarations

    subroutine create_declaration_nodes(arena, prog, prog_index, var_names, &
                                        var_types, var_declared, var_count, &
                                        declaration_indices)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer, intent(in) :: prog_index
        character(len=64), intent(in) :: var_names(:)
        character(len=64), intent(in) :: var_types(:)
        logical, intent(in) :: var_declared(:)
        integer, intent(in) :: var_count
        integer, allocatable, intent(out) :: declaration_indices(:)
        type(declaration_node) :: decl_node
        integer :: i, decl_idx, actual_count

        actual_count = 0
        do i = 1, var_count
            if (var_declared(i)) then
                if (.not. has_explicit_declaration(arena, prog, var_names(i))) then
                    actual_count = actual_count + 1
                end if
            end if
        end do

        if (actual_count == 0) then
            allocate (declaration_indices(0))
            return
        end if

        allocate (declaration_indices(actual_count))

        decl_idx = 0
        do i = 1, var_count
            if (var_declared(i)) then
                if (.not. has_explicit_declaration(arena, prog, var_names(i))) then
                    decl_idx = decl_idx + 1
                    call create_single_declaration(arena, prog_index, var_names(i), &
                                                   var_types(i), decl_node)
                    call arena%push(decl_node, "declaration", prog_index)
                    declaration_indices(decl_idx) = arena%size
                end if
            end if
        end do
    end subroutine create_declaration_nodes

    subroutine create_single_declaration(arena, prog_index, var_name, &
                                         var_type, decl_node)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: var_name, var_type
        type(declaration_node), intent(out) :: decl_node

        decl_node%uid = generate_uid()
        decl_node%var_name = trim(var_name)
        decl_node%has_kind = .false.
        decl_node%initializer_index = 0
        decl_node%line = 1
        decl_node%column = 1
        decl_node%is_array = .false.
        decl_node%is_allocatable = .false.
        decl_node%disable_grouping = .false.

        call apply_type_string_to_decl(arena, prog_index, var_name, var_type, &
                                       decl_node)
    end subroutine create_single_declaration

    logical function has_explicit_declaration(arena, prog, var_name)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        character(len=*), intent(in) :: var_name
        integer :: i, j
        character(len=64) :: target_name
        character(len=64) :: candidate_name

        target_name = to_lower(trim(var_name))

        has_explicit_declaration = .false.

        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. &
                    prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (declaration_node)
                            candidate_name = to_lower(trim(stmt%var_name))
                            if (trim(candidate_name) == trim(target_name)) then
                                has_explicit_declaration = .true.
                                return
                            end if
                            if (stmt%is_multi_declaration .and. &
                                allocated(stmt%var_names)) then
                                do j = 1, size(stmt%var_names)
                                    candidate_name = to_lower(trim(stmt%var_names(j)))
                                    if (trim(candidate_name) == trim(target_name)) then
                                        has_explicit_declaration = .true.
                                        return
                                    end if
                                end do
                            end if
                        end select
                    end if
                end if
            end do
        end if
    end function has_explicit_declaration

end module standardizer_declarations_insertion
