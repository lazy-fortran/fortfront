module standardizer_declarations_core
    ! Core variable declaration generation module
    ! Handles implicit none insertion, variable declaration creation

    use iso_fortran_env, only: error_unit
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_nodes_data
    use ast_nodes_misc
    use ast_nodes_bounds, only: range_expression_node
    use ast_nodes_procedure
    use ast_nodes_loops
    use ast_nodes_control
    use ast_nodes_io
    use uid_generator, only: generate_uid
    use ast_factory
    use type_system_unified
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use error_handling, only: result_t, success_result, create_error_result
    use lexer_core, only: to_lower
    use standardizer_types
    use intrinsic_registry, only: get_intrinsic_signature, is_intrinsic_function
    use string_utils_mod, only: int_to_string
    use type_string_utils, only: is_character_type_string, mono_type_to_string
    use ast_nodes_misc, only: comment_node
    implicit none
    private

    ! Type standardization configuration (local copy)
    ! DISABLED: Converting real -> real(8) breaks generic interfaces that
    ! depend on exact type matching. Users should explicitly use real(8) or
    ! kind parameters if they want double precision.
    logical, save :: standardizer_type_standardization_enabled = .false.

    public :: insert_variable_declarations
    public :: has_implicit_none
    public :: program_has_variable_declarations
    public :: find_declaration_insertion_point
    public :: generate_and_insert_declarations
    public :: has_explicit_declaration
    public :: standardize_declarations
    public :: collect_statement_vars
    public :: collect_assignment_vars
    public :: collect_identifier_var
    public :: collect_identifier_var_with_type
    public :: add_variable
    public :: mark_variable_declared
    public :: handle_string_concatenation
    public :: get_string_length_from_node
    public :: infer_type_from_binary_operation
    public :: get_standardizer_type_standardization

contains

    ! Get type standardization setting
    subroutine get_standardizer_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = standardizer_type_standardization_enabled
    end subroutine get_standardizer_type_standardization

    ! Insert variable declarations and implicit none for a program
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

        ! Find insertion point (after use statements, before executable statements)
        implicit_insert_pos = find_declaration_insertion_point(arena, prog)
        ! Default to beginning when no use statements are present
        if (implicit_insert_pos == 0) then
            implicit_insert_pos = 1
        end if
        header_insert_pos = find_declaration_header_end(arena, prog)
        if (header_insert_pos < implicit_insert_pos) then
            header_insert_pos = implicit_insert_pos
        end if

        ! Check if implicit none already exists
        if (.not. has_implicit_none(arena, prog)) then
            ! Create implicit none statement node
            implicit_none_index = push_implicit_statement(arena, .true., &
                                                          line=1, column=1, &
                                                          parent_index=prog_index)
        else
            implicit_none_index = 0  ! Don't add duplicate
        end if

        ! Collect and generate variable declarations for any missing variables
        call generate_and_insert_declarations(arena, prog, prog_index, &
                                              declaration_indices)
        n_declarations = 0
        if (allocated(declaration_indices)) n_declarations = size(declaration_indices)

        ! Create new body indices with optional implicit none and declarations
        total_extra = n_declarations
        if (implicit_none_index > 0) total_extra = total_extra + 1
        allocate (new_body_indices(size(prog%body_indices) + total_extra))

        ! Copy use statements
        j = 1
        if (implicit_insert_pos > 1) then
            do i = 1, implicit_insert_pos - 1
                new_body_indices(j) = prog%body_indices(i)
                j = j + 1
            end do
        end if

        ! Insert implicit none if we created one
        if (implicit_none_index > 0) then
            new_body_indices(j) = implicit_none_index
            j = j + 1
        end if

        ! Determine header statements to retain before new declarations
        header_copy_end = header_insert_pos - 1
        if (header_copy_end >= implicit_insert_pos) then
            do while (header_copy_end >= implicit_insert_pos)
                if (.not. is_header_separator(header_copy_end)) exit
                header_copy_end = header_copy_end - 1
            end do
        else
            header_copy_end = implicit_insert_pos - 1
        end if
        separator_start = header_copy_end + 1
        if (separator_start < implicit_insert_pos) separator_start = &
            implicit_insert_pos

        ! Copy retained header statements (uses are already handled)
        if (header_copy_end >= implicit_insert_pos) then
            do i = implicit_insert_pos, header_copy_end
                new_body_indices(j) = prog%body_indices(i)
                j = j + 1
            end do
        end if

        ! Insert newly generated declarations, if any
        do i = 1, n_declarations
            new_body_indices(j) = declaration_indices(i)
            j = j + 1
        end do

        ! Reinsert trailing separators (comments/blank lines) between header and body
        if (separator_start <= header_insert_pos - 1) then
            do i = separator_start, header_insert_pos - 1
                new_body_indices(j) = prog%body_indices(i)
                j = j + 1
            end do
        end if

        ! Copy remaining statements
        if (header_insert_pos <= size(prog%body_indices)) then
            do i = header_insert_pos, size(prog%body_indices)
                new_body_indices(j) = prog%body_indices(i)
                j = j + 1
            end do
        end if

        ! Update program body
        prog%body_indices = new_body_indices

        ! Update the arena entry
        arena%entries(prog_index)%node = prog

    contains

        logical function is_header_separator(pos)
            integer, intent(in) :: pos
            integer :: node_index

            is_header_separator = .false.
            if (.not. allocated(prog%body_indices)) return
            if (pos < 1 .or. pos > size(prog%body_indices)) return

            node_index = prog%body_indices(pos)
            if (node_index <= 0 .or. node_index > arena%size) return
            if (.not. allocated(arena%entries(node_index)%node)) return

            select type (stmt => arena%entries(node_index)%node)
            type is (comment_node)
                ! Legacy statement comments are NOT separators, they are declarations
                is_header_separator = .not. is_legacy_statement_comment(stmt)
            type is (blank_line_node)
                is_header_separator = .true.
            class default
                is_header_separator = .false.
            end select
        end function is_header_separator

    end subroutine insert_variable_declarations

    ! Check if a program already has implicit none or any implicit statement
    function has_implicit_none(arena, prog) result(found)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        logical :: found
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
                        ! Return true for any IMPLICIT statement (including type ranges)
                        ! to prevent adding duplicate implicit none
                        found = .true.
                        return
                    end select
                end if
            end if
        end do
    end function has_implicit_none

    ! Check if program already has variable declarations (indicating it's been standardized)
    function program_has_variable_declarations(arena, prog) result(has_decls)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        logical :: has_decls
        integer :: i

        has_decls = .false.
        if (.not. allocated(prog%body_indices)) return
        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (declaration_node)
                        ! Found a variable declaration - program is already standardized
                        has_decls = .true.
                        return
                    type is (parameter_declaration_node)
                        ! Found a parameter declaration - also indicates standardization
                        has_decls = .true.
                        return
                    end select
                end if
            end if
        end do
    end function program_has_variable_declarations

    ! Check if a comment node represents a legacy statement (COMMON, EQUIVALENCE, BLOCK)
    logical function is_legacy_statement_comment(node)
        type(comment_node), intent(in) :: node
        character(len=:), allocatable :: lowered_text

        is_legacy_statement_comment = .false.
        if (.not. allocated(node%text)) return

        lowered_text = to_lower(adjustl(trim(node%text)))
        if (len_trim(lowered_text) >= 11) then
            if (index(lowered_text, "equivalence") == 1) then
                is_legacy_statement_comment = .true.
                return
            end if
        end if
        if (len_trim(lowered_text) >= 6) then
            if (index(lowered_text, "common") == 1) then
                is_legacy_statement_comment = .true.
                return
            end if
        end if
        if (len_trim(lowered_text) >= 5) then
            if (index(lowered_text, "block") == 1) then
                is_legacy_statement_comment = .true.
                return
            end if
        end if
    end function is_legacy_statement_comment

    integer function find_prefix_end(arena, prog, mode) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer, intent(in) :: mode
        integer :: i
        logical :: keep_scanning

        pos = 1
        if (.not. allocated(prog%body_indices)) return

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    keep_scanning = .false.
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (use_statement_node)
                        keep_scanning = .true.
                    type is (comment_node)
                        ! Legacy statement comments (COMMON/EQUIVALENCE) are treated as declarations
                        if (is_legacy_statement_comment(stmt)) then
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

    ! Find where to insert declarations (after use statements)
    function find_declaration_insertion_point(arena, prog) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: pos

        pos = find_prefix_end(arena, prog, 1)
    end function find_declaration_insertion_point

    ! Find the position after existing declaration header statements
    function find_declaration_header_end(arena, prog) result(pos)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        integer :: pos

        pos = find_prefix_end(arena, prog, 2)
    end function find_declaration_header_end

    ! Standardize existing declarations (e.g., real -> real(8))
    subroutine standardize_declarations(arena, prog)
        type(ast_arena_t), intent(inout) :: arena
        type(program_node), intent(in) :: prog
        integer :: i
        logical :: standardizer_type_standardization_enabled

        if (.not. allocated(prog%body_indices)) return

        call get_standardizer_type_standardization( &
            standardizer_type_standardization_enabled)

        do i = 1, size(prog%body_indices)
            if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= arena%size) then
                if (allocated(arena%entries(prog%body_indices(i))%node)) then
                    select type (stmt => arena%entries(prog%body_indices(i))%node)
                    type is (declaration_node)
                        ! Standardize the type name (if enabled)
                        if (stmt%type_name == "real" .and. &
                            standardizer_type_standardization_enabled) then
                            stmt%type_name = "real"
                            stmt%has_kind = .true.
                            stmt%kind_value = 8
                        end if
                        ! Update the node in the arena
                        arena%entries(prog%body_indices(i))%node = stmt
                    end select
                end if
            end if
        end do
    end subroutine standardize_declarations

    ! Generate and insert variable declarations from inferred types
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
        var_declared = .false.
        var_count = 0
        func_count = 0

        ! First pass: collect function names
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (function_def_node)
                            if (func_count < size(function_names)) then
                                func_count = func_count + 1
                                function_names(func_count) = stmt%name
                            end if
                        end select
                    end if
                end if
            end do
        end if

        ! Collect all variables that need declarations
        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. prog%body_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        call collect_statement_vars(arena, prog%body_indices(i), &
                                                    var_names, var_types, &
                                                    var_declared, &
                                                    var_count, &
                                                    function_names, func_count)
                    end if
                end if
            end do
        end if

        ! Update existing declarations with inferred types
        if (var_count > 0) then
            if (allocated(prog%body_indices)) then
                do i = 1, var_count
                    if (len_trim(var_names(i)) == 0) cycle
                    if (len_trim(var_types(i)) == 0) cycle
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

        ! Create declaration nodes
        if (var_count > 0) then
            call create_declaration_nodes(arena, prog, prog_index, var_names, &
                                          var_types, &
                                          var_declared, var_count, declaration_indices)
        else
            allocate (declaration_indices(0))
        end if

    end subroutine generate_and_insert_declarations

    ! Create declaration nodes from collected variables
    subroutine create_declaration_nodes(arena, prog, prog_index, var_names, &
                                        var_types, &
                                        var_declared, var_count, declaration_indices)
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

        ! First count how many declarations we'll actually create
        actual_count = 0
        do i = 1, var_count
            if (var_declared(i)) then
                ! Check if this variable already has an explicit declaration
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

        ! Now create the declaration nodes
        decl_idx = 0
        do i = 1, var_count
            if (var_declared(i)) then
                ! Check if this variable already has an explicit declaration
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

    ! Create a single declaration node
    subroutine create_single_declaration(arena, prog_index, var_name, &
                                         var_type, decl_node)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: var_name, var_type
        type(declaration_node), intent(out) :: decl_node

        ! Initialize declaration node
        decl_node%uid = generate_uid()
        decl_node%var_name = trim(var_name)
        decl_node%has_kind = .false.
        decl_node%initializer_index = 0
        decl_node%line = 1
        decl_node%column = 1
        decl_node%is_array = .false.
        decl_node%is_allocatable = .false.

        call apply_type_string_to_decl(arena, prog_index, var_name, &
                                       var_type, decl_node)

    end subroutine create_single_declaration

    subroutine apply_type_string_to_decl(arena, prog_index, var_name, &
                                         var_type, decl_node)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: var_name, var_type
        type(declaration_node), intent(inout) :: decl_node
        integer :: dim_pos
        logical :: has_dimension_attr
        character(len=:), allocatable :: lowered_type
        character(len=:), allocatable :: base_part
        character(len=:), allocatable :: attr_part
        character(len=:), allocatable :: filtered_attr
        character(len=:), allocatable :: attr_trim
        character(len=:), allocatable :: component
        character(len=:), allocatable :: lowered_component
        integer :: comma_pos, paren_pos, close_pos
        integer :: kind_val, ios
        integer :: comp_start, comp_end, local_comma

        has_dimension_attr = .false.
        lowered_type = to_lower(var_type)

        comma_pos = index(var_type, ',')
        if (comma_pos > 0) then
            base_part = trim(var_type(1:comma_pos - 1))
            attr_part = trim(var_type(comma_pos + 1:))
        else
            base_part = trim(var_type)
            attr_part = ''
        end if

        decl_node%has_kind = .false.
        decl_node%kind_value = 0

        paren_pos = index(base_part, '(')
        if (paren_pos > 0) then
            close_pos = index(base_part(paren_pos:), ')')
            if (close_pos > 0) then
                close_pos = paren_pos + close_pos - 1
                read (base_part(paren_pos + 1:close_pos - 1), *, iostat=ios) kind_val
                if (ios == 0) then
                    if (index(to_lower(base_part(1:paren_pos - 1)), &
                              'character') == 0) then
                        decl_node%has_kind = .true.
                        decl_node%kind_value = kind_val
                        base_part = trim(base_part(1:paren_pos - 1))
                    end if
                end if
            end if
        end if

        decl_node%type_name = trim(base_part)
        filtered_attr = ""
        if (len_trim(attr_part) > 0) then
            attr_trim = trim(attr_part)
            if (len_trim(attr_trim) > 0) then
                lowered_component = to_lower(attr_trim)
                do
                    dim_pos = index(lowered_component, 'dimension(')
                    if (dim_pos == 0) exit
                    comp_start = dim_pos
                    comp_end = comp_start + len('dimension(')
                    local_comma = 1
                    do while (comp_end <= len(lowered_component) .and. local_comma > 0)
                        select case (lowered_component(comp_end:comp_end))
                        case ('(')
                            local_comma = local_comma + 1
                        case (')')
                            local_comma = local_comma - 1
                        end select
                        comp_end = comp_end + 1
                    end do
                    comp_end = comp_end - 1
                    if (comp_end < comp_start) exit
                    attr_trim = attr_trim(:comp_start - 1) // attr_trim(comp_end + 1:)
                    lowered_component = to_lower(attr_trim)
                end do

                attr_trim = trim(attr_trim)
                if (len_trim(attr_trim) > 0) then
                    comp_start = 1
                    do
                        if (comp_start > len_trim(attr_trim)) exit
                        local_comma = index(attr_trim(comp_start:), ',')
                        if (local_comma > 0) then
                            comp_end = comp_start + local_comma - 2
                        else
                            comp_end = len_trim(attr_trim)
                        end if
                        if (comp_end >= comp_start) then
                            component = trim(attr_trim(comp_start:comp_end))
                            if (len_trim(component) > 0) then
                                if (len_trim(filtered_attr) > 0) then
                                    filtered_attr = filtered_attr // ', '
                                end if
                                filtered_attr = filtered_attr // component
                            end if
                        end if
                        if (local_comma == 0) exit
                        comp_start = comp_end + 2
                    end do
                end if
            end if
        end if
        if (len_trim(filtered_attr) > 0) then
            decl_node%type_name = trim(decl_node%type_name) // ', ' // &
                                  trim(filtered_attr)
        end if

        dim_pos = index(lowered_type, 'dimension(')
        if (dim_pos > 0) then
            has_dimension_attr = .true.
            call parse_dimension_attribute(arena, prog_index, var_type, &
                                           dim_pos, decl_node)
        else
            if (allocated(decl_node%dimension_indices)) then
                deallocate (decl_node%dimension_indices)
            end if
            decl_node%is_array = .false.
        end if

        if (index(lowered_type, 'allocatable') > 0) then
            decl_node%is_allocatable = .true.
        else if (.not. has_dimension_attr) then
            decl_node%is_allocatable = .false.
        end if

        if (.not. has_dimension_attr) then
            call set_array_properties_from_type(arena, var_name, prog_index, decl_node)
        end if

        if (decl_node%is_array .and. allocated(decl_node%dimension_indices)) then
            if (size(decl_node%dimension_indices) > 0) then
                if (decl_node%dimension_indices(1) == 0) then
                    decl_node%is_allocatable = .true.
                end if
            end if
        end if
    end subroutine apply_type_string_to_decl

    subroutine update_existing_declaration_type(arena, prog_index, var_name, var_type)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: var_name, var_type
        type(program_node) :: prog
        integer :: i, j, node_idx

        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do i = 1, size(prog%body_indices)
                node_idx = prog%body_indices(i)
                if (node_idx <= 0 .or. node_idx > arena%size) cycle
                if (.not. allocated(arena%entries(node_idx)%node)) cycle
                select type (decl => arena%entries(node_idx)%node)
                type is (declaration_node)
                    if (.not. decl%is_multi_declaration) then
                        if (trim(decl%var_name) == trim(var_name)) then
                            call apply_type_string_to_decl(arena, prog_index, &
                                                           var_name, &
                                                           var_type, decl)
                            arena%entries(node_idx)%node = decl
                            return
                        end if
                    else if (allocated(decl%var_names)) then
                        do j = 1, size(decl%var_names)
                            if (trim(decl%var_names(j)) == trim(var_name)) then
                                if (index(to_lower(var_type), 'dimension(') == 0) then
                                    call apply_type_string_to_decl(arena, prog_index, &
                                                                   var_name, &
                                                                   var_type, decl)
                                    arena%entries(node_idx)%node = decl
                                end if
                                return
                            end if
                        end do
                    end if
                end select
            end do
        end select
    end subroutine update_existing_declaration_type

    ! Parse dimension attribute from type string
    subroutine parse_dimension_attribute(arena, prog_index, var_type, &
                                         dim_pos, decl_node)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: prog_index, dim_pos
        character(len=*), intent(in) :: var_type
        type(declaration_node), intent(inout) :: decl_node
        integer :: paren_pos, iostat, dim_size, i, comma_count, ndims
        integer :: start_pos, end_pos, comma_pos
        character(len=20) :: dim_str
        type(literal_node) :: size_literal
        character(len=20) :: size_str
        integer, allocatable :: dimensions(:)
        character(len=100) :: dims_str
        logical :: has_explicit_bounds
        integer :: dim_idx

        ! Check if declaration already has explicit bounds coming from parsing
        has_explicit_bounds = .false.
        if (allocated(decl_node%dimension_indices)) then
            if (size(decl_node%dimension_indices) > 0) then
                has_explicit_bounds = .true.
                do i = 1, size(decl_node%dimension_indices)
                    dim_idx = decl_node%dimension_indices(i)
                    if (dim_idx <= 0) then
                        has_explicit_bounds = .false.
                        exit
                    else if (dim_idx > arena%size) then
                        cycle
                    else if (.not. allocated(arena%entries(dim_idx)%node)) then
                        has_explicit_bounds = .false.
                        exit
                    else
                        select type (dim_node => arena%entries(dim_idx)%node)
                        type is (range_expression_node)
                            if (dim_node%start_index <= 0 .or. &
                                dim_node%end_index <= 0) then
                                has_explicit_bounds = .false.
                                exit
                            end if
                        class default
                            cycle
                        end select
                    end if
                end do
            end if
        end if

        if (decl_node%is_parameter) then
            ! Parameter constants must preserve explicit bounds from the parser.
            ! The inferred type string may report deferred shape.
            ! That is invalid for PARAMETER entities, so leave existing indices.
            decl_node%is_array = .true.
            return
        end if

        ! Extract dimension values from the inferred type string
        paren_pos = index(var_type(dim_pos:), ')')
        if (paren_pos > 10) then  ! Must have at least 1 character after dimension(
            dims_str = var_type(dim_pos + 10:dim_pos + paren_pos - 2)
        else
            ! No valid dimension specification in type string
            if (has_explicit_bounds) then
                ! Preserve existing dimension information from parsing phase
                decl_node%is_array = .true.
                decl_node%is_allocatable = .false.
            end if
            return
        end if

        ! Check if it's a deferred shape (:) - indicates allocatable
        if (trim(dims_str) == ':') then
            ! If we have explicit bounds from parsing (e.g., arr(0:9)), preserve them (fixes #1812)
            if (has_explicit_bounds) then
                decl_node%is_array = .true.
                decl_node%is_allocatable = .false.
                return
            end if
            decl_node%is_array = .true.
            decl_node%is_allocatable = .true.
            if (allocated(decl_node%dimension_indices)) &
                deallocate (decl_node%dimension_indices)
            allocate (decl_node%dimension_indices(1))
            decl_node%dimension_indices(1) = 0  ! 0 indicates deferred shape
            return
        end if

        ! Count commas to determine number of dimensions
        comma_count = 0
        do i = 1, len_trim(dims_str)
            if (dims_str(i:i) == ',') comma_count = comma_count + 1
        end do
        ndims = comma_count + 1

        allocate (dimensions(ndims))

        ! Parse each dimension
        start_pos = 1
        do i = 1, ndims
            if (i < ndims) then
                comma_pos = index(dims_str(start_pos:), ',')
                if (comma_pos > 0) then
                    end_pos = start_pos + comma_pos - 2
                else
                    end_pos = len_trim(dims_str)
                end if
            else
                end_pos = len_trim(dims_str)
            end if

            dim_str = dims_str(start_pos:end_pos)
            read (dim_str, *, iostat=iostat) dim_size
            if (iostat == 0) then
                dimensions(i) = dim_size
            else
                dimensions(i) = 0  ! Failed to parse
            end if

            start_pos = end_pos + 2  ! Skip comma
        end do

        ! Compare with existing explicit bounds if present
        if (has_explicit_bounds) then
            ! Check if dimensions match - only update if they differ
            if (size(decl_node%dimension_indices) == ndims) then
                block
                    logical :: dimensions_match, dimensions_known
                    integer :: existing_dim, dim_idx
                    dimensions_match = .true.
                    dimensions_known = .true.
                    do i = 1, ndims
                        dim_idx = decl_node%dimension_indices(i)
                        if (dim_idx <= 0 .or. dim_idx > arena%size) cycle
                        if (.not. allocated(arena%entries(dim_idx)%node)) then
                            dimensions_known = .false.
                            exit
                        end if
                        select type (dim_node => arena%entries(dim_idx)%node)
                        type is (literal_node)
                            read (dim_node%value, *, iostat=iostat) existing_dim
                            if (iostat /= 0) then
                                dimensions_known = .false.
                                exit
                            end if
                            if (existing_dim /= dimensions(i)) then
                                dimensions_match = .false.
                                exit
                            end if
                        class default
                            dimensions_known = .false.
                            exit
                        end select
                    end do
                    if (.not. dimensions_known) then
                        ! Cannot prove mismatch, preserve explicit bounds
                        deallocate (dimensions)
                        return
                    end if
                    if (dimensions_match) then
                        ! Dimensions match, preserve existing bounds
                        deallocate (dimensions)
                        return
                    end if
                end block
            end if
        end if

        ! Update or set dimension indices with inferred values
        decl_node%is_array = .true.
        if (allocated(decl_node%dimension_indices)) &
            deallocate (decl_node%dimension_indices)
        allocate (decl_node%dimension_indices(ndims))

        ! Create literal nodes for each dimension
        do i = 1, ndims
            if (dimensions(i) > 0) then
                write (size_str, '(i0)') dimensions(i)
                size_literal%uid = generate_uid()
                size_literal%value = trim(size_str)
                size_literal%literal_kind = LITERAL_INTEGER
                size_literal%line = 1
                size_literal%column = 1
                call arena%push(size_literal, "literal", prog_index)
                decl_node%dimension_indices(i) = arena%size
            else
                decl_node%dimension_indices(i) = 0  ! Deferred shape
            end if
        end do

        deallocate (dimensions)

    end subroutine parse_dimension_attribute

    ! Set array properties from inferred type information
    subroutine set_array_properties_from_type(arena, var_name, prog_index, decl_node)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: prog_index
        type(declaration_node), intent(inout) :: decl_node
        integer :: j, i
        type(literal_node) :: size_literal
        character(len=20) :: size_str
        type(mono_type_t) :: current_type
        integer :: ndims, dim_idx
        integer, allocatable :: dim_sizes(:)

        if (decl_node%is_parameter) then
            ! Parameter constants must keep explicit bounds from parsing.
            return
        end if

        ! Search for the identifier node with this name to check its inferred type
        do j = 1, arena%size
            if (allocated(arena%entries(j)%node)) then
                select type (node => arena%entries(j)%node)
                type is (identifier_node)
                    if (trim(node%name) == trim(var_name)) then
                        if (node%inferred_type%kind > 0) then
                            if (node%inferred_type%kind == TARRAY) then
                                decl_node%is_array = .true.

                                ! Count array dimensions for nested arrays

                                current_type = node%inferred_type
                                ndims = 0

                                ! Count dimensions by traversing nested array types
                                do while (current_type%kind == TARRAY)
                                    ndims = ndims + 1
                                    if (.not. current_type%has_args() .or. &
                                        current_type%get_args_count() < 1) exit
                                    current_type = current_type%get_arg(1)
                                end do

                                ! For a nested array (2D), standardize as regular 2D array
                                if (ndims > 1) then
                                    ndims = 2  ! For [[1,2],[3,4]] -> 2x2 array
                                end if

                                ! Allocate dimension indices
                                if (allocated(decl_node%dimension_indices)) &
                                    deallocate (decl_node%dimension_indices)
                                allocate (decl_node%dimension_indices(ndims))
                                allocate (dim_sizes(ndims))

                                ! Extract dimension sizes
                                ! For nested arrays, we need to get both dimensions
                                if (ndims == 2) then
                                    ! First dimension = outer array size (number of sub-arrays)
                                    dim_sizes(1) = node%inferred_type%size
                                    ! Second dimension = inner array size (elements per sub-array)
                                    if (node%inferred_type%has_args() .and. &
                                        node%inferred_type%get_args_count() > 0) then
                                        current_type = node%inferred_type%get_arg(1)
                                        if (current_type%kind == TARRAY) then
                                            dim_sizes(2) = current_type%size
                                        else
                                            dim_sizes(2) = 0  ! Should not happen for nested arrays
                                        end if
                                    else
                                        dim_sizes(2) = 0
                                    end if
                                else
                                    ! Single dimension array
                                    current_type = node%inferred_type
                                    dim_idx = 1
                                    do while (current_type%kind == TARRAY .and. &
                                              dim_idx <= ndims)
                                        dim_sizes(dim_idx) = current_type%size
                                        if (.not. current_type%has_args() .or. &
                                            current_type%get_args_count() < 1) exit
                                        current_type = current_type%get_arg(1)
                                        dim_idx = dim_idx + 1
                                    end do
                                end if

                                ! Set dimension indices, storing literals in arena
                                do i = 1, ndims
                                    if (dim_sizes(i) > 0) then
                                        if (.not. node%inferred_type%alloc_info% &
                                            is_allocatable) then
                                            write (size_str, '(i0)') dim_sizes(i)
                                            size_literal%uid = generate_uid()
                                            size_literal%value = trim(size_str)
                                            size_literal%literal_kind = LITERAL_INTEGER
                                            size_literal%line = 1
                                            size_literal%column = 1
                                            call arena%push(size_literal, "literal", &
                                                            prog_index)
                                            decl_node%dimension_indices(i) = arena%size
                                        else
                                            decl_node%dimension_indices(i) = 0
                                        end if
                                    else
                                        decl_node%dimension_indices(i) = 0
                                    end if
                                end do

                                deallocate (dim_sizes)
                                exit
                            end if
                        end if
                    end if
                end select
            end if
        end do

    end subroutine set_array_properties_from_type

    ! Check if variable has explicit declaration
    function has_explicit_declaration(arena, prog, var_name) result(has_decl)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        character(len=*), intent(in) :: var_name
        logical :: has_decl
        integer :: i, j

        has_decl = .false.

        if (allocated(prog%body_indices)) then
            do i = 1, size(prog%body_indices)
                if (prog%body_indices(i) > 0 .and. &
                    prog%body_indices(i) <= arena%size) then
                    if (allocated(arena%entries(prog%body_indices(i))%node)) then
                        select type (stmt => arena%entries(prog%body_indices(i))%node)
                        type is (declaration_node)
                            ! Check single variable declaration
                            if (trim(stmt%var_name) == trim(var_name)) then
                                has_decl = .true.
                                return
                            end if
                            ! Check multi-variable declaration
                            if (stmt%is_multi_declaration .and. &
                                allocated(stmt%var_names)) then
                                do j = 1, size(stmt%var_names)
                                    if (trim(stmt%var_names(j)) == trim(var_name)) then
                                        has_decl = .true.
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

    ! Collect variables from statement
    subroutine collect_statement_vars(arena, stmt_index, var_names, &
                                      var_types, var_declared, var_count, &
                                      function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count

        type stack_entry
            integer :: idx = 0
        end type stack_entry

        type(stack_entry), allocatable :: stack(:)
        integer :: capacity, top
        integer :: current_index
        integer :: j

        capacity = 128
        allocate (stack(capacity))
        top = 0

        call push(stmt_index)

        do while (top > 0)
            current_index = pop()
            if (current_index <= 0 .or. current_index > arena%size) cycle
            if (.not. allocated(arena%entries(current_index)%node)) cycle

            select type (stmt => arena%entries(current_index)%node)
            type is (declaration_node)
                if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                    do j = 1, size(stmt%var_names)
                        call register_decl_var(trim(stmt%var_names(j)), stmt)
                    end do
                else
                    call register_decl_var(trim(stmt%var_name), stmt)
                end if
            type is (assignment_node)
                call collect_assignment_vars(arena, current_index, var_names, &
                                             var_types, var_declared, var_count, &
                                             function_names, func_count)
            type is (do_loop_node)
                call add_variable(stmt%var_name, "integer", var_names, var_types, &
                                  var_declared, var_count, function_names, func_count)
                if (allocated(stmt%body_indices)) call push_many(stmt%body_indices)
            type is (do_while_node)
                if (allocated(stmt%body_indices)) call push_many(stmt%body_indices)
            type is (io_implied_do_node)
                call add_variable(stmt%var_name, "integer", var_names, var_types, &
                                  var_declared, var_count, function_names, func_count)
                if (stmt%expr_index > 0) call push(stmt%expr_index)
            type is (if_node)
                if (allocated(stmt%else_body_indices)) call &
                    push_many(stmt%else_body_indices)
                if (allocated(stmt%then_body_indices)) call &
                    push_many(stmt%then_body_indices)
            type is (select_case_node)
                if (stmt%selector_index > 0) call push(stmt%selector_index)
                if (allocated(stmt%case_indices)) call push_many(stmt%case_indices)
                if (stmt%default_index > 0) call push(stmt%default_index)
            type is (case_block_node)
                if (allocated(stmt%body_indices)) call push_many(stmt%body_indices)
            type is (case_default_node)
                if (allocated(stmt%body_indices)) call push_many(stmt%body_indices)
            type is (print_statement_node)
                if (allocated(stmt%expression_indices)) then
                    call push_many(stmt%expression_indices)
                end if
            type is (read_statement_node)
                if (allocated(stmt%var_indices)) then
                    call push_many(stmt%var_indices)
                end if
            type is (identifier_node)
                call collect_identifier_var(stmt, var_names, var_types, &
                                            var_declared, var_count, &
                                            function_names, func_count)
            class default
                ! other nodes handled elsewhere as needed
            end select
        end do

    contains

        subroutine register_decl_var(name, decl)
            character(len=*), intent(in) :: name
            type(declaration_node), intent(in) :: decl
            character(len=:), allocatable :: type_str
            integer :: idx, k

            if (len_trim(name) == 0) return
            type_str = declaration_type_string(decl)
            call add_variable(name, type_str, var_names, var_types, var_declared, &
                              var_count, function_names, func_count)
            call mark_variable_declared(name, var_names, var_declared, var_count)

            idx = 0
            do k = 1, var_count
                if (trim(var_names(k)) == trim(name)) then
                    idx = k
                    exit
                end if
            end do
            if (idx > 0 .and. len_trim(type_str) > 0) then
                var_types(idx) = type_str
            end if
        end subroutine register_decl_var

        function declaration_type_string(decl) result(type_str)
            type(declaration_node), intent(in) :: decl
            character(len=:), allocatable :: type_str
            character(len=32) :: buffer
            integer :: dim_idx, i

            type_str = trim(decl%type_name)
            if (decl%has_kind) then
                buffer = int_to_string(decl%kind_value)
                if (len_trim(buffer) > 0) then
                    type_str = trim(type_str) // "(" // trim(buffer) // ")"
                end if
            end if

            if (decl%is_array .and. allocated(decl%dimension_indices)) then
                type_str = trim(type_str) // ", dimension("
                do i = 1, size(decl%dimension_indices)
                    if (i > 1) type_str = type_str // ","
                    dim_idx = decl%dimension_indices(i)
                    if (dim_idx == 0) then
                        type_str = type_str // ":"
                    else if (dim_idx > 0 .and. dim_idx <= arena%size) then
                        if (allocated(arena%entries(dim_idx)%node)) then
                            select type (dim_node => arena%entries(dim_idx)%node)
                            type is (literal_node)
                                type_str = type_str // trim(dim_node%value)
                            class default
                                type_str = type_str // ":"
                            end select
                        else
                            type_str = type_str // ":"
                        end if
                    else if (dim_idx > arena%size) then
                        buffer = int_to_string(dim_idx)
                        type_str = type_str // trim(buffer)
                    else
                        type_str = type_str // ":"
                    end if
                end do
                type_str = type_str // ")"
            end if

            if (decl%is_allocatable) then
                if (.not. has_attribute(type_str, "allocatable")) then
                    type_str = trim(type_str) // ", allocatable"
                end if
            end if

            if (decl%is_pointer) then
                if (.not. has_attribute(type_str, "pointer")) then
                    type_str = trim(type_str) // ", pointer"
                end if
            end if

            if (decl%is_target) then
                if (.not. has_attribute(type_str, "target")) then
                    type_str = trim(type_str) // ", target"
                end if
            end if

            if (decl%is_parameter) then
                if (.not. has_attribute(type_str, "parameter")) then
                    type_str = trim(type_str) // ", parameter"
                end if
            end if

            if (decl%has_intent .and. allocated(decl%intent)) then
                if (.not. has_attribute(type_str, "intent(")) then
                    type_str = trim(type_str) // ", intent(" // &
                        trim(decl%intent) // ")"
                end if
            end if
        end function declaration_type_string

        pure logical function has_attribute(text, attr) result(found)
            character(len=*), intent(in) :: text
            character(len=*), intent(in) :: attr
            character(len=:), allocatable :: lowered
            integer :: i, char_code

            lowered = trim(text)
            do i = 1, len(lowered)
                char_code = iachar(lowered(i:i))
                if (char_code >= iachar('A') .and. char_code <= iachar('Z')) then
                    lowered(i:i) = achar(char_code + 32)
                end if
            end do
            found = index(lowered, trim(attr)) > 0
        end function has_attribute

        subroutine push(idx)
            integer, intent(in) :: idx
            type(stack_entry), allocatable :: tmp(:)
            if (idx <= 0) return
            if (top >= capacity) then
                allocate (tmp(capacity * 2))
                if (capacity > 0) tmp(1:capacity) = stack(1:capacity)
                call move_alloc(tmp, stack)
                capacity = size(stack)
            end if
            top = top + 1
            stack(top)%idx = idx
        end subroutine push

        subroutine push_many(indices)
            integer, intent(in) :: indices(:)
            integer :: k
            do k = size(indices), 1, -1
                call push(indices(k))
            end do
        end subroutine push_many

        integer function pop()
            if (top <= 0) then
                pop = 0
            else
                pop = stack(top)%idx
                top = top - 1
            end if
        end function pop

    end subroutine collect_statement_vars

    ! Collect variables from assignment
    subroutine collect_assignment_vars(arena, assign_index, var_names, &
                                       var_types, var_declared, var_count, &
                                       function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: assign_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        type(mono_type_t), pointer :: value_type
        character(len=64) :: var_type
        integer :: existing_idx
        integer :: i
        integer :: literal_length

        if (assign_index <= 0 .or. assign_index > arena%size) return
        if (.not. allocated(arena%entries(assign_index)%node)) return

        select type (assign => arena%entries(assign_index)%node)
        type is (assignment_node)
            ! Get target node
            if (assign%target_index > 0 .and. assign%target_index <= arena%size) then
                if (allocated(arena%entries(assign%target_index)%node)) then
                    select type (target => arena%entries(assign%target_index)%node)
                    type is (identifier_node)
                        ! Try to get type from the value expression
                        var_type = ""  ! Empty default indicates type not determined

                        ! Check if variable was already collected (second assignment)
                        existing_idx = 0
                        do i = 1, var_count
                            if (trim(var_names(i)) == trim(target%name)) then
                                existing_idx = i
                                exit
                            end if
                        end do

                        if (assign%value_index > 0 .and. &
                            assign%value_index <= arena%size) then
                            if (allocated(arena%entries(assign%value_index)%node)) then
                                ! Check if it's an array expression by structure
                                if (is_array_expression(arena, &
                                                        assign%value_index)) then
                                    ! Try to determine array size if possible
                                    var_type = &
                                        get_array_var_type(arena, assign%value_index)
                                else
                                    ! First try to get type from general expression type system
                                    value_type => &
                                        get_expression_type(arena, assign%value_index)
                                    if (associated(value_type)) then
                                        block
                                            type(string_result_t) :: type_result
                                            type_result = &
                                                get_fortran_type_string(value_type)
                                            if (type_result%is_success()) then
                                                var_type = type_result%get_value()
                                            end if
                                        end block
                                    end if

                                    ! If get_expression_type didn't work, check for intrinsic functions
                                    if (len_trim(var_type) == 0) then
                                        call infer_type_from_intrinsic_call( &
                                            arena, assign%value_index, var_type)
                                    end if

                                    ! Prefer integer for pure-integer binary expressions
                                    if (len_trim(var_type) == 0) then
                                        if (is_integer_expression( &
                                            arena, assign%value_index)) then
                                            var_type = "integer"
                                        end if
                                    end if

                                    ! If that failed, check for string concatenation as special case
                                    if (len_trim(var_type) == 0) then
                                        var_type = handle_string_concatenation( &
                                                   arena, assign%value_index)
                                    end if

                                    ! If still no type found, try to infer from binary operation structure
                                    if (len_trim(var_type) == 0) then
                                        var_type = infer_type_from_binary_operation( &
                                                   arena, assign%value_index)
                                    end if
                                end if
                            end if
                        end if

                        if (len_trim(var_type) == 0) then
                            literal_length = get_string_length_from_node( &
                                             arena, assign%value_index)
                            if (literal_length >= 0) then
                                var_type = build_character_type_from_length( &
                                           literal_length)
                            end if
                        end if

                        if (len_trim(var_type) == 0) then
                            var_type = "real"  ! Default for mathematical expressions
                        end if

                        ! If this is a subsequent assignment to the same variable, only mark
                        ! as allocatable for character strings needing deferred length
                        if (existing_idx > 0) then
                            if (len_trim(var_type) > 0) then
                                if (is_character_type_string( &
                                    var_types(existing_idx)) .and. &
                                    is_character_type_string(var_type)) then
                                    var_types(existing_idx) = &
                                        merge_character_type_lengths( &
                                        var_types(existing_idx), var_type)
                                else
                                    var_types(existing_idx) = trim(var_type)
                                end if
                            end if
                            if (index(var_types(existing_idx), 'character(') &
                                == 1 .and. &
                                index(var_types(existing_idx), 'len=:') > 0 .and. &
                                index(var_types(existing_idx), &
                                      'allocatable') == 0) then
                                var_types(existing_idx) = &
                                    trim(var_types(existing_idx)) &
                                    // ", allocatable"
                            end if
                        else
                            ! Now collect the variable with the determined type
                            call collect_identifier_var_with_type(target, var_type, &
                                                                  var_names, &
                                                                  var_types, &
                                                                  var_declared, &
                                                                  var_count, &
                                                                  function_names, &
                                                                  func_count)
                        end if
                    type is (call_or_subscript_node)
                        if (target%is_array_access .and. allocated(target%name)) then
                            block
                                character(len=64) :: base_name
                                character(len=96) :: decl_type
                                integer :: rank, idx

                                base_name = trim(target%name)
                                decl_type = ''

                                if (assign%type_was_inferred .and. &
                                    allocated(assign%inferred_type_name)) then
                                    decl_type = trim(assign%inferred_type_name)
                                end if

                                if (len_trim(decl_type) == 0) then
                                    rank = 0
                                    if (allocated(target%arg_indices)) rank = &
                                        size(target%arg_indices)
                                    if (rank <= 0) rank = 1
                                    decl_type = 'real, dimension('
                                    do idx = 1, rank
                                        if (idx > 1) decl_type = trim(decl_type) // ','
                                        decl_type = trim(decl_type) // ':'
                                    end do
                                    decl_type = trim(decl_type) // ')'
                                end if

                                call add_variable(base_name, decl_type, var_names, &
                                                  var_types, &
                                                  var_declared, var_count, &
                                                  function_names, func_count)
                            end block
                        end if
                    end select
                end if
            end if
        end select
    end subroutine collect_assignment_vars

    ! Add variable to collections
    subroutine add_variable(var_name, var_type, var_names, var_types, &
                            var_declared, var_count, &
                            function_names, func_count)
        character(len=*), intent(in) :: var_name, var_type
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i
        logical :: found, is_function

        ! Check if this is a function name
        is_function = .false.
        do i = 1, func_count
            if (trim(function_names(i)) == trim(var_name)) then
                is_function = .true.
                exit
            end if
        end do

        ! Skip if it's a function
        if (is_function) return

        ! Check if variable already exists
        found = .false.
        do i = 1, var_count
            if (trim(var_names(i)) == trim(var_name)) then
                found = .true.
                exit
            end if
        end do

        if (.not. found) then
            var_count = var_count + 1
            if (var_count <= size(var_names)) then
                var_names(var_count) = var_name
                var_types(var_count) = var_type
                var_declared(var_count) = .true.
            end if
        end if
    end subroutine add_variable

    ! Mark variable as declared
    subroutine mark_variable_declared(var_name, var_names, var_declared, var_count)
        character(len=*), intent(in) :: var_name
        character(len=64), intent(in) :: var_names(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(in) :: var_count
        integer :: i

        ! Find the variable if it exists and mark it as declared
        do i = 1, var_count
            if (trim(var_names(i)) == trim(var_name)) then
                var_declared(i) = .false.  ! Mark as already declared - don't generate implicit declaration
                return
            end if
        end do
    end subroutine mark_variable_declared

    ! Collect identifier variable with type
    subroutine collect_identifier_var_with_type(identifier, var_type, &
                                                var_names, var_types, var_declared, &
                                                var_count, &
                                                function_names, func_count)
        type(identifier_node), intent(in) :: identifier
        character(len=*), intent(in) :: var_type
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count

        call add_variable(identifier%name, var_type, var_names, var_types, &
                          var_declared, var_count, function_names, func_count)
    end subroutine collect_identifier_var_with_type

    ! Basic type inference functions
    function handle_string_concatenation(arena, expr_index) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=64) :: var_type

        var_type = ""  ! Default to empty (not a string concatenation)

        ! Check if the expression is actually a string concatenation
        if (expr_index > 0 .and. expr_index <= arena%size) then
            if (allocated(arena%entries(expr_index)%node)) then
                select type (node => arena%entries(expr_index)%node)
                type is (binary_op_node)
                    ! Only string concatenation uses the // operator
                    if (node%operator == "//") then
                        var_type = "character(len=:), allocatable"
                    end if
                end select
            end if
        end if
    end function handle_string_concatenation

    function infer_type_from_binary_operation(arena, expr_index) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=64) :: var_type
        if (is_integer_expression(arena, expr_index)) then
            var_type = "integer"
        else
            var_type = "real"
        end if
    end function infer_type_from_binary_operation

    ! Heuristic: determine if expression consists of integer-only operations
    logical function is_integer_expression(arena, idx) result(is_int)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: idx
        integer, allocatable :: node_stack(:)
        integer :: top, cap
        integer :: current
        logical :: ok

        is_int = .false.
        if (idx <= 0 .or. idx > arena%size) return
        if (.not. allocated(arena%entries(idx)%node)) return

        cap = 16
        allocate (node_stack(cap))
        top = 0

        call push(idx)
        is_int = .true.

        loop_nodes: do while (top > 0)
            current = node_stack(top)
            top = top - 1

            if (current <= 0 .or. current > arena%size) then
                is_int = .false.
                exit loop_nodes
            end if
            if (.not. allocated(arena%entries(current)%node)) then
                is_int = .false.
                exit loop_nodes
            end if

            select type (node => arena%entries(current)%node)
            type is (literal_node)
                if (node%literal_kind /= LITERAL_INTEGER) then
                    is_int = .false.
                    exit loop_nodes
                end if
            type is (identifier_node)
                if (node%inferred_type%kind > 0) then
                    if (node%inferred_type%kind /= TINT) then
                        is_int = .false.
                        exit loop_nodes
                    end if
                end if
            type is (binary_op_node)
                if (trim(node%operator) == "/") then
                    is_int = .false.
                    exit loop_nodes
                end if
                ok = push_if_valid(node%left_index)
                if (.not. ok) then
                    is_int = .false.
                    exit loop_nodes
                end if
                ok = push_if_valid(node%right_index)
                if (.not. ok) then
                    is_int = .false.
                    exit loop_nodes
                end if
            class default
                is_int = .false.
                exit loop_nodes
            end select
        end do loop_nodes

        if (allocated(node_stack)) deallocate (node_stack)

    contains

        subroutine push(index)
            integer, intent(in) :: index
            if (top >= cap) call grow_stack()
            top = top + 1
            node_stack(top) = index
        end subroutine push

        logical function push_if_valid(index) result(success)
            integer, intent(in) :: index
            success = .false.
            if (index <= 0 .or. index > arena%size) return
            if (.not. allocated(arena%entries(index)%node)) return
            call push(index)
            success = .true.
        end function push_if_valid

        subroutine grow_stack()
            integer, allocatable :: tmp(:)
            allocate (tmp(cap * 2))
            tmp(1:cap) = node_stack(1:cap)
            call move_alloc(tmp, node_stack)
            cap = cap * 2
        end subroutine grow_stack

    end function is_integer_expression

    ! Collect identifier variable
    subroutine collect_identifier_var(identifier, var_names, var_types, &
                                      var_declared, var_count, &
                                      function_names, func_count)
        type(identifier_node), intent(in) :: identifier
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        character(len=:), allocatable :: inferred_type
        logical :: success_flag
        logical :: standardize_flag

        standardize_flag = standardizer_type_standardization_enabled

        if (identifier%inferred_type%kind > 0) then
            inferred_type = mono_type_to_string( &
                            identifier%inferred_type, include_shape=.false., &
                            prefer_len_zero_char=.true., &
                            standardize_real=standardize_flag, &
                            success=success_flag)
            if (success_flag) then
                if (len_trim(inferred_type) > 0) then
                    call collect_identifier_var_with_type( &
                        identifier, trim(inferred_type), var_names, var_types, &
                        var_declared, var_count, function_names, func_count)
                    return
                end if
            end if
        end if

        call add_variable(identifier%name, "real", var_names, var_types, &
                          var_declared, var_count, function_names, func_count)
    end subroutine collect_identifier_var

    recursive function get_string_length_from_node(arena, node_index) result(length)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer :: length
        integer :: left_len
        integer :: right_len

        length = -1
        if (node_index <= 0) return
        if (node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (literal_node)
            if (node%literal_kind == LITERAL_STRING) then
                if (allocated(node%value)) then
                    length = compute_string_literal_length(node%value)
                else
                    length = 0
                end if
            end if
        type is (binary_op_node)
            if (allocated(node%operator)) then
                if (trim(node%operator) == "//") then
                    left_len = get_string_length_from_node(arena, &
                                                           node%left_index)
                    right_len = get_string_length_from_node(arena, &
                                                            node%right_index)
                    if (left_len >= 0 .and. right_len >= 0) then
                        length = left_len + right_len
                    end if
                end if
            end if
        class default
            length = -1
        end select
    end function get_string_length_from_node

    subroutine infer_type_from_intrinsic_call(arena, value_index, var_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: value_index
        character(len=64), intent(inout) :: var_type
        character(len=:), allocatable :: intrinsic_sig

        if (len_trim(var_type) > 0) return
        if (value_index <= 0) return
        if (value_index > arena%size) return
        if (.not. allocated(arena%entries(value_index)%node)) return

        select type (val_node => arena%entries(value_index)%node)
        type is (call_or_subscript_node)
            if (.not. is_intrinsic_function(val_node%name)) return
            intrinsic_sig = get_intrinsic_signature(val_node%name)
            if (len_trim(intrinsic_sig) == 0) return

            if (index(intrinsic_sig, "real(") == 1) then
                var_type = "real"
            else if (index(intrinsic_sig, "integer(") == 1) then
                var_type = "integer"
            else if (index(intrinsic_sig, "logical(") == 1) then
                var_type = "logical"
            else if (index(intrinsic_sig, "character(") == 1) then
                var_type = "character(len=:), allocatable"
            else
                var_type = "real"
            end if
        end select
    end subroutine infer_type_from_intrinsic_call

    pure function build_character_type_from_length(len_value) result(type_str)
        integer, intent(in) :: len_value
        character(len=64) :: type_str
        character(len=32) :: buffer

        type_str = ""
        if (len_value < 0) return

        write (buffer, '(i0)') len_value
        type_str = "character(len=" // trim(buffer) // ")"
    end function build_character_type_from_length

    pure function merge_character_type_lengths(existing, incoming) result(result_type)
        character(len=*), intent(in) :: existing
        character(len=*), intent(in) :: incoming
        character(len=64) :: result_type
        integer :: existing_len
        integer :: incoming_len

        if (is_deferred_character_length(existing) .or. &
            is_deferred_character_length(incoming)) then
            result_type = "character(len=:), allocatable"
            return
        end if

        existing_len = extract_character_length(existing)
        incoming_len = extract_character_length(incoming)

        if (incoming_len < 0) then
            result_type = trim(existing)
        else if (existing_len < 0) then
            result_type = build_character_type_from_length(incoming_len)
        else
            result_type = build_character_type_from_length( &
                          max(existing_len, incoming_len))
        end if
    end function merge_character_type_lengths

    pure integer function extract_character_length(type_str) result(len_value)
        character(len=*), intent(in) :: type_str
        character(len=:), allocatable :: lowered
        integer :: len_pos
        integer :: close_pos
        integer :: ios

        len_value = -1
        lowered = to_lower(adjustl(trim(type_str)))
        if (len(lowered) == 0) return
        if (index(lowered, "character") /= 1) return

        len_pos = index(lowered, "len=")
        if (len_pos <= 0) return
        if (len_pos + 4 > len(lowered)) return

        if (lowered(len_pos + 4:len_pos + 4) == ':' .or. &
            lowered(len_pos + 4:len_pos + 4) == '*') then
            len_value = -1
            return
        end if

        close_pos = index(lowered(len_pos:), ")")
        if (close_pos <= 0) return
        if (len_pos + close_pos - 2 < len_pos + 4) return

        read (lowered(len_pos + 4:len_pos + close_pos - 2), *, iostat=ios) len_value
        if (ios /= 0) len_value = -1
    end function extract_character_length

    pure logical function is_deferred_character_length(type_str) result(is_deferred)
        character(len=*), intent(in) :: type_str
        character(len=:), allocatable :: lowered
        integer :: len_pos

        lowered = to_lower(adjustl(trim(type_str)))
        len_pos = index(lowered, "len=")
        if (len_pos <= 0) then
            is_deferred = .false.
            return
        end if

        if (len_pos + 4 > len(lowered)) then
            is_deferred = .false.
            return
        end if

        is_deferred = (lowered(len_pos + 4:len_pos + 4) == ':' .or. &
                       lowered(len_pos + 4:len_pos + 4) == '*')
    end function is_deferred_character_length

    pure integer function compute_string_literal_length(literal) result(len_value)
        character(len=*), intent(in) :: literal
        character(len=:), allocatable :: trimmed_literal
        character(len=:), allocatable :: inner
        character(len=1) :: quote_char
        integer :: trimmed_len
        integer :: i

        trimmed_literal = adjustl(trim(literal))
        trimmed_len = len_trim(trimmed_literal)
        if (trimmed_len < 2) then
            len_value = trimmed_len
            return
        end if

        quote_char = trimmed_literal(1:1)
        if (quote_char /= '"' .and. quote_char /= "'") then
            len_value = trimmed_len
            return
        end if

        if (trimmed_literal(trimmed_len:trimmed_len) /= quote_char) then
            len_value = trimmed_len
            return
        end if

        if (trimmed_len == 2) then
            len_value = 0
            return
        end if

        inner = trimmed_literal(2:trimmed_len - 1)
        len_value = len(inner)

        i = 1
        do while (i <= len(inner) - 1)
            if (inner(i:i) == quote_char .and. inner(i + 1:i + 1) == quote_char) then
                len_value = len_value - 1
                i = i + 2
            else
                i = i + 1
            end if
        end do
    end function compute_string_literal_length

end module standardizer_declarations_core
