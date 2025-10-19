module codegen_declarations_program_mod
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node, identifier_node, literal_node, &
                              assignment_node, array_literal_node, call_or_subscript_node
    use ast_nodes_misc, only: blank_line_node, comment_node, contains_node, &
                              implicit_statement_node, use_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_data, only: declaration_node
    use string_utils_mod, only: int_to_string, to_lower
    use type_string_utils, only: mono_type_to_string
    use codegen_utilities, only: generate_grouped_body_context
    use codegen_arena_interface, only: generate_code_from_arena
    implicit none
    private
    public :: generate_code_program

contains

    ! Generate code for program nodes
    function generate_code_program(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code
        integer :: i, j
        logical :: in_contains_section
        logical :: found_contains
        logical :: has_non_trivial_body
        logical :: context_has_executable_before_contains
        integer, allocatable :: non_use_indices(:)
        integer :: non_use_count

        context_has_executable_before_contains = .false.
        non_use_count = 0

        ! Check if there's a non-trivial body before contains
        has_non_trivial_body = .false.
        found_contains = .false.
        if (allocated(node%body_indices)) then
            do i = 1, size(node%body_indices)
                if (node%body_indices(i) > 0 .and. node%body_indices(i) <= &
                    arena%size) then
                    if (allocated(arena%entries(node%body_indices(i))%node)) then
                        select type (body_node => &
                                     arena%entries(node%body_indices(i))%node)
                        type is (contains_node)
                            found_contains = .true.
                            exit
                        type is (comment_node)
                            ! Comments don't count as non-trivial
                        type is (blank_line_node)
                            ! Blank lines don't count as non-trivial
                        class default
                            has_non_trivial_body = .true.
                        end select
                    end if
                end if
            end do
        end if

        context_has_executable_before_contains = has_non_trivial_body .and. &
                                                 found_contains

        ! Handle special multi-unit container
        if (node%name == "__MULTI_UNIT__") then
            ! Generate code for each unit as siblings without program wrapper
            code = ""
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0 .and. node%body_indices(i) <= &
                        arena%size) then
                        if (allocated(arena%entries(node%body_indices(i))%node)) then
                            select type (child => &
                                         arena%entries(node%body_indices(i))%node)
                            type is (program_node)
                                ! Skip trivial implicit main wrappers that only contain comments/blank lines
                                if (program_is_trivial_wrapper(arena, &
                                                               node%body_indices(i), &
                                                               child%name)) then
                                    block
                                        character(len=:), allocatable :: trivia_code
                                        trivia_code = &
                                            collect_trivial_program_trivia(arena, &
                                                                     node%body_indices(i))
                                        if (len_trim(trivia_code) > 0) then
                                            if (len(code) > 0) code = code // &
                                                                      new_line('A') // &
                                                                      new_line('A')
                                            code = code // trivia_code
                                        end if
                                    end block
                                    cycle
                                end if
                            type is (subroutine_def_node)
                                ! Skip duplicate empty subroutines (defensive check)
                                if (.not. allocated(child%body_indices) .or. &
                                    size(child%body_indices) == 0) then
                                    if (.not. allocated(child%param_indices) .or. &
                                        size(child%param_indices) == 0) then
                                        ! Check if this is a duplicate of a previous subroutine
                                        block
                                            integer :: j
                                            logical :: is_duplicate
                                            is_duplicate = .false.
                                            do j = 1, i - 1
                                                if (node%body_indices(j) > 0 .and. &
                                                    node%body_indices(j) <= &
                                                    arena%size) then
                             if (allocated(arena%entries(node%body_indices(j))%node)) then
                                                        select type (prev => &
                                                 arena%entries(node%body_indices(j))%node)
                                                        type is (subroutine_def_node)
                                                            if (prev%name == &
                                                                child%name) then
                                                                is_duplicate = .true.
                                                                exit
                                                            end if
                                                        end select
                                                    end if
                                                end if
                                            end do
                                            if (is_duplicate) cycle
                                        end block
                                    end if
                                end if
                            end select
                        end if
                        if (len(code) > 0) then
                            code = code // new_line('A') // new_line('A')
                        end if
                        code = code // generate_code_from_arena(arena, &
                                                                node%body_indices(i))
                    end if
                end do
            end if
            return
        end if

        ! Program header
        code = "program " // node%name // new_line('A')

        ! Process use statements first, then add implicit none, then rest of body
        block
            logical :: has_implicit
            logical :: is_use_stmt
            character(len=:), allocatable :: use_statements_code
            character(len=:), allocatable :: loop_var_declarations
            character(len=:), allocatable :: extra_decls

            has_implicit = .false.
            use_statements_code = ""
            loop_var_declarations = ""

            ! First pass: collect use statements and check for implicit none
            if (allocated(node%body_indices)) then
                allocate (non_use_indices(size(node%body_indices)))
                non_use_count = 0

                do i = 1, size(node%body_indices)
                    if (node%body_indices(i) > 0 .and. node%body_indices(i) <= &
                        arena%size) then
                        if (allocated(arena%entries(node%body_indices(i))%node)) then
                            is_use_stmt = .false.

                            select type (ib => &
                                         arena%entries(node%body_indices(i))%node)
                            type is (use_statement_node)
                                ! Generate use statement code
                                is_use_stmt = .true.
                                use_statements_code = use_statements_code // "    " // &
                                                      generate_code_from_arena(arena, &
                                                                node%body_indices(i)) // &
                                                      new_line('A')

                            type is (implicit_statement_node)
                                if (ib%is_none) has_implicit = .true.
                                non_use_count = non_use_count + 1
                                non_use_indices(non_use_count) = node%body_indices(i)

                            type is (literal_node)
                                if (allocated(ib%value)) then
                                    if (index(ib%value, 'implicit none') > 0) &
                                        has_implicit = .true.
                                end if
                                non_use_count = non_use_count + 1
                                non_use_indices(non_use_count) = node%body_indices(i)

                            class default
                                non_use_count = non_use_count + 1
                                non_use_indices(non_use_count) = node%body_indices(i)
                            end select

                            ! Don't add use statements to non_use_indices
                            if (is_use_stmt) then
                                ! Use statement already processed, skip
                            end if
                        end if
                    end if
                end do
            end if

            ! Add use statements first
            if (len(use_statements_code) > 0) then
                code = code // use_statements_code
            end if

            ! Then add implicit none if not present
            if (.not. has_implicit) then
                code = code // "    implicit none" // new_line('A')
            end if

            ! Collect and add variable declarations for undeclared identifiers
            extra_decls = collect_program_variable_decls(arena, node)
            if (len_trim(extra_decls) > 0) then
                code = code // extra_decls
            end if
        end block

        ! Generate rest of body (non-use statements) with proper grouping
        if (allocated(node%body_indices) .and. non_use_count > 0) then
            body_code = generate_grouped_body_with_context(arena, &
                                                       non_use_indices(1:non_use_count), &
                                                           1, &
                                                   context_has_executable_before_contains)

            if (index(body_code, 'output_unit') > 0) then
                block
                    integer :: search_pos
                    integer :: iso_pos
                    integer :: line_start
                    integer :: line_end
                    integer :: header_end
                    integer :: comment_pos
                    logical :: has_iso_line
                    logical :: iso_has_only
                    logical :: iso_has_output
                    character(len=:), allocatable :: prefix
                    character(len=:), allocatable :: suffix
                    character(len=:), allocatable :: iso_line
                    character(len=:), allocatable :: iso_comment
                    character(len=:), allocatable :: trimmed_line

                    has_iso_line = .false.
                    search_pos = 1

                    do
                        iso_pos = index(code(search_pos:), 'iso_fortran_env')
                        if (iso_pos == 0) exit
                        iso_pos = search_pos + iso_pos - 1

                        line_start = iso_pos
                        do while (line_start > 1 .and. code(line_start - &
                                                            1:line_start - 1) &
                                  /= new_line('A'))
                            line_start = line_start - 1
                        end do

                        line_end = iso_pos
                        do while (line_end <= len(code) .and. code(line_end:line_end) &
                                  /= new_line('A'))
                            line_end = line_end + 1
                        end do

                        has_iso_line = .true.

                        if (line_end > len(code)) then
                            iso_line = code(line_start:)
                        else
                            iso_line = code(line_start:line_end - 1)
                        end if

                        iso_has_only = index(to_lower(iso_line), 'only:') > 0
                        iso_has_output = index(to_lower(iso_line), &
                                               'output_unit') > 0

                        if (iso_has_only .and. .not. iso_has_output) then
                            if (line_start > 1) then
                                prefix = code(1:line_start - 1)
                            else
                                prefix = ''
                            end if

                            if (line_end <= len(code)) then
                                if (line_end < len(code)) then
                                    suffix = code(line_end + 1:)
                                else
                                    suffix = ''
                                end if
                            else
                                suffix = ''
                            end if

                            comment_pos = scan(iso_line, '!')
                            if (comment_pos > 0) then
                                if (comment_pos > 1) then
                                    trimmed_line = iso_line(1:comment_pos - 1)
                                else
                                    trimmed_line = ''
                                end if
                                iso_comment = iso_line(comment_pos:)
                            else
                                trimmed_line = iso_line
                                iso_comment = ''
                            end if

                            if (len_trim(trimmed_line) > 0) then
                                trimmed_line = trimmed_line(1:len_trim(trimmed_line))
                            end if

                            iso_line = trimmed_line // ', output_unit'
                            if (len_trim(iso_comment) > 0) then
                                iso_line = iso_line // ' ' // iso_comment
                            end if

                            code = prefix // iso_line // new_line('A') // suffix
                            iso_has_output = .true.
                        end if

                        if (.not. iso_has_only .or. iso_has_output) exit

                        if (line_end <= len(code)) then
                            search_pos = line_end + 1
                        else
                            exit
                        end if
                    end do

                    if (.not. has_iso_line) then
                        header_end = index(code, new_line('A'))
                        if (header_end <= 0) header_end = len(code)

                        if (header_end > 0) then
                            prefix = code(1:header_end)
                        else
                            prefix = ''
                        end if

                        if (header_end < len(code)) then
                            suffix = code(header_end + 1:)
                        else
                            suffix = ''
                        end if

                        code = prefix // &
                           '    use, intrinsic :: iso_fortran_env, only: output_unit' // &
                               new_line('A') // suffix
                    end if
                end block
            end if

            ! Check if body contains implied do loops and add loop variables after implicit none
            if (len(body_code) > 0) then
                block
                    integer :: pos, start_pos, end_pos, impl_pos, insert_pos
                    character(len=:), allocatable :: before_code, after_code, var_name
                    character(len=:), allocatable :: loop_vars(:)
                    integer :: n_vars, i, j
                    logical :: already_declared

                    ! Find all implied do loop variables
                    allocate (character(len=32) :: loop_vars(20))  ! Support up to 20 loop variables
                    n_vars = 0

                    ! Search for patterns like "(var=" in implied do loops (both old and new syntax)
                    pos = 1
                    do while (pos <= len(body_code))
                        ! Find next occurrence of either "= (/(" or "= [(", with or without spaces
                        start_pos = index(body_code(pos:), "= (/(")
                        if (start_pos == 0) then
                            start_pos = index(body_code(pos:), "= (/ (")
                        end if
                        if (start_pos == 0) then
                            ! Try new syntax
                            start_pos = index(body_code(pos:), "= [(")
                            if (start_pos > 0) then
                                start_pos = pos + start_pos - 1
                                ! Find the end with "]" for new syntax
                                end_pos = index(body_code(start_pos:), ")]")
                                if (end_pos > 0) then
                                    end_pos = start_pos + end_pos - 1
                                    ! Extract variables from this implied do section
                       call extract_loop_vars_from_section(body_code(start_pos:end_pos), &
                                                                        loop_vars, &
                                                                        n_vars)
                                end if
                                pos = start_pos + 3  ! Move past "= [("
                            else
                                exit  ! No more patterns found
                            end if
                        else
                            start_pos = pos + start_pos - 1
                            ! Find the loop variable patterns for old syntax
                            end_pos = index(body_code(start_pos:), "/)")
                            if (end_pos > 0) then
                                end_pos = start_pos + end_pos - 1
                                ! Extract variables from this implied do section
                       call extract_loop_vars_from_section(body_code(start_pos:end_pos), &
                                                                    loop_vars, n_vars)
                            end if
                            pos = start_pos + 5  ! Move past "= (/("
                        end if
                    end do

                    ! If we found loop variables, add declarations
                    if (n_vars > 0 .or. (index(body_code, "[(") > 0 .and. &
                                         index(body_code, ")]") > 0)) then
                        ! Check if implicit none is in body_code
                        impl_pos = index(body_code, "implicit none")
                        if (impl_pos > 0) then
                            ! Find the end of the implicit none line
                            insert_pos = impl_pos + 13  ! Length of "implicit none"
                            do while (insert_pos <= len(body_code))
                                if (body_code(insert_pos:insert_pos) == &
                                    new_line('A')) then
                                    insert_pos = insert_pos + 1
                                    exit
                                end if
                                insert_pos = insert_pos + 1
                            end do

                            ! Build declarations for loop variables
                            before_code = body_code(1:insert_pos - 1)
                            after_code = body_code(insert_pos:)

                            if (n_vars > 0) then
                                do i = 1, n_vars
                                    ! Skip if already declared
                                    already_declared = .false.
                                    if (index(body_code, &
                                              "integer :: "//trim(loop_vars(i))) > 0) then
                                        already_declared = .true.
                                    end if

                                    if (.not. already_declared) then
                                        before_code = before_code // &
                                                      "    integer :: " // &
                                                      trim(loop_vars(i)) // new_line('A')
                                    end if
                                end do
                            else
                                ! Check for implied do with default i
                                if (index(body_code, "[(") > 0 .and. index(body_code, &
                                                                           ")]") > 0) then
                                    if (index(body_code, "integer :: i") == 0) then
                                        before_code = before_code // "    integer :: i" &
                                                      // new_line('A')
                                    end if
                                end if
                            end if

                            body_code = before_code // after_code
                        else
                            ! No implicit none in body, add to code as before
                            if (n_vars > 0) then
                                do i = 1, n_vars
                                    already_declared = .false.
                                    if (index(body_code, &
                                              "integer :: "//trim(loop_vars(i))) > 0) then
                                        already_declared = .true.
                                    end if
                                    if (index(code, &
                                              "integer :: "//trim(loop_vars(i))) > 0) then
                                        already_declared = .true.
                                    end if

                                    if (.not. already_declared) then
                                        code = code // "    integer :: " // &
                                               trim(loop_vars(i)) // new_line('A')
                                    end if
                                end do
                            else
                                if (index(body_code, "[(") > 0 .and. index(body_code, &
                                                                           ")]") > 0) then
                                    if (index(body_code, "integer :: i") == 0 .and. &
                                        index(code, "integer :: i") == 0) then
                                        code = code // "    integer :: i" // &
                                               new_line('A')
                                    end if
                                end if
                            end if
                        end if
                    end if
                end block
            end if

            code = code // body_code
        end if

        if (allocated(non_use_indices)) then
            deallocate (non_use_indices)
        end if

        ! Program end
        code = code // "end program " // node%name

    end function generate_code_program

    logical function program_is_trivial_wrapper(arena, prog_index, name) &
        result(is_trivial)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: name
        integer :: j, child_idx

        is_trivial = .false.
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. (trim(name) == 'main' .or. trim(name) == &
                       '__IMPLICIT_MAIN__')) return
            if (.not. allocated(prog%body_indices) .or. &
                size(prog%body_indices) == 0) then
                is_trivial = .true.
                return
            end if

            is_trivial = .true.
            do j = 1, size(prog%body_indices)
                child_idx = prog%body_indices(j)
                if (child_idx <= 0 .or. child_idx > arena%size) cycle
                if (.not. allocated(arena%entries(child_idx)%node)) cycle
                select type (body => arena%entries(child_idx)%node)
                type is (comment_node)
                    cycle
                type is (blank_line_node)
                    cycle
                type is (implicit_statement_node)
                    if (body%is_none) cycle
                    is_trivial = .false.
                    return
                class default
                    is_trivial = .false.
                    return
                end select
            end do
        class default
            return
        end select
    end function program_is_trivial_wrapper

    function collect_trivial_program_trivia(arena, prog_index) result(trivia_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable :: trivia_code
        integer :: j, child_idx
        character(len=:), allocatable :: snippet

        trivia_code = ""
        if (prog_index <= 0 .or. prog_index > arena%size) return
        if (.not. allocated(arena%entries(prog_index)%node)) return

        select type (prog => arena%entries(prog_index)%node)
        type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do j = 1, size(prog%body_indices)
                child_idx = prog%body_indices(j)
                if (child_idx <= 0 .or. child_idx > arena%size) cycle
                if (.not. allocated(arena%entries(child_idx)%node)) cycle
                select type (body => arena%entries(child_idx)%node)
                type is (comment_node)
                    snippet = generate_code_from_arena(arena, child_idx)
                type is (blank_line_node)
                    snippet = generate_code_from_arena(arena, child_idx)
                class default
                    cycle
                end select

                if (len(snippet) > 0) then
                    if (len(trivia_code) > 0) trivia_code = trivia_code // new_line('A')
                    trivia_code = trivia_code // snippet
                end if
            end do
        end select
    end function collect_trivial_program_trivia

    ! Generate grouped body with context
    function generate_grouped_body_with_context(arena, body_indices, indent, &
                                                has_exec_before_contains) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        logical, intent(in) :: has_exec_before_contains
        character(len=:), allocatable :: code

        ! Pass context to utilities module
        code = generate_grouped_body_context(arena, body_indices, indent, &
                                             has_exec_before_contains)
    end function generate_grouped_body_with_context

    ! Helper subroutine to extract loop variables from an implied do section
    subroutine extract_loop_vars_from_section(section, loop_vars, n_vars)
        character(len=*), intent(in) :: section
        character(len=*), intent(inout) :: loop_vars(:)
        integer, intent(inout) :: n_vars
        integer :: pos, eq_pos, comma_pos
        character(len=32) :: var_name
        logical :: already_added
        integer :: i

        ! Look for patterns like "i=1," or "j=1," or "k=1,"
        pos = 1
        do while (pos < len_trim(section))
            eq_pos = index(section(pos:), "=")
            if (eq_pos == 0) exit
            eq_pos = pos + eq_pos - 1

            ! Look backwards from = to find variable name
            if (eq_pos > 1) then
                ! Find the start of the variable name
                i = eq_pos - 1
                do while (i > 0)
                    if (section(i:i) == ' ' .or. section(i:i) == ',' .or. &
                        section(i:i) == '(') then
                        exit
                    end if
                    i = i - 1
                end do

                ! Extract variable name
                var_name = adjustl(trim(section(i + 1:eq_pos - 1)))

                ! Check if it looks like a loop variable (single letter or simple name)
                if (len_trim(var_name) > 0 .and. len_trim(var_name) <= 8) then
                    ! Check if it's a number after =
                    comma_pos = index(section(eq_pos + 1:), ",")
                    if (comma_pos > 0) then
                        ! This looks like a loop variable
                        ! Check if already in list
                        already_added = .false.
                        do i = 1, n_vars
                            if (trim(loop_vars(i)) == trim(var_name)) then
                                already_added = .true.
                                exit
                            end if
                        end do

                        if (.not. already_added .and. n_vars < size(loop_vars)) then
                            n_vars = n_vars + 1
                            loop_vars(n_vars) = trim(var_name)
                        end if
                    end if
                end if
            end if

            pos = eq_pos + 1
        end do
    end subroutine extract_loop_vars_from_section

    ! Collect variable declarations for undeclared identifiers in programs
    function collect_program_variable_decls(arena, prog) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        character(len=:), allocatable :: decl_code
        integer, parameter :: MAX_VARS = 256
        character(len=64) :: declared_names(MAX_VARS)
        character(len=64) :: var_names(MAX_VARS)
        character(len=64) :: var_types(MAX_VARS)
        character(len=64) :: func_names(MAX_VARS)
        character(len=64) :: func_types(MAX_VARS)
        character(len=64) :: internal_funcs(MAX_VARS)
        character(len=64) :: func_return_type
        character(len=64) :: defined_func_names(MAX_VARS)
        character(len=64) :: defined_func_types(MAX_VARS)
        logical :: name_declared
        integer :: declared_count, var_count, func_count, internal_count
        integer :: i, j, idx, target_idx
        integer :: defined_func_count
        character(len=64) :: name_buf
        character(len=:), allocatable :: type_buf

        decl_code = ""
        declared_count = 0
        var_count = 0
        func_count = 0
        internal_count = 0
        defined_func_count = 0

        declared_names = ""
        var_names = ""
        var_types = ""
        func_names = ""
        func_types = ""
        internal_funcs = ""
        func_return_type = ""
        defined_func_names = ""
        defined_func_types = ""

        if (.not. allocated(prog%body_indices)) return

        call build_function_return_type_table(arena, defined_func_names, &
                                              defined_func_types, defined_func_count)

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (decl => arena%entries(idx)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    do j = 1, size(decl%var_names)
                        if (declared_count < MAX_VARS) then
                            declared_count = declared_count + 1
                            declared_names(declared_count) = trim(decl%var_names(j))
                        end if
                    end do
                else
                    if (declared_count < MAX_VARS) then
                        declared_count = declared_count + 1
                        declared_names(declared_count) = trim(decl%var_name)
                    end if
                end if
            type is (function_def_node)
                if (internal_count < MAX_VARS) then
                    internal_count = internal_count + 1
                    internal_funcs(internal_count) = trim(decl%name)
                end if
            end select
        end do

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (assignment_node)
                target_idx = stmt%target_index
                if (target_idx > 0 .and. target_idx <= arena%size) then
                    if (allocated(arena%entries(target_idx)%node)) then
                        select type (id => arena%entries(target_idx)%node)
                        type is (identifier_node)
                            name_buf = trim(id%name)
                            if (len_trim(name_buf) == 0) cycle
                            name_declared = exists_in_list(declared_names, &
                                                           declared_count, name_buf)
                            if (.not. name_declared) then
                                if (.not. exists_in_list(var_names, var_count, &
                                                         name_buf)) then
                                    type_buf = mono_type_to_string( &
                                        id%inferred_type, include_shape=.true., &
                                        fallback='real')
                                    if (len_trim(type_buf) == 0 .or. &
                                        trim(type_buf) == 'real') then
                                        func_return_type = ''
                                        if (stmt%value_index > 0 .and. &
                                            stmt%value_index <= arena%size) then
                                            if (allocated(arena%entries( &
                                                          stmt%value_index)%node)) then
                                                select type (rhs => arena%entries( &
                                                             stmt%value_index)%node)
                                                type is (call_or_subscript_node)
                                                    if (len_trim(rhs%name) > 0) then
                                                        func_return_type = &
                                                            lookup_function_return_type( &
                                                            defined_func_names, &
                                                            defined_func_types, &
                                                            defined_func_count, &
                                                            rhs%name)
                                                  if (len_trim(func_return_type) > 0) then
                                                            type_buf = trim( &
                                                                func_return_type)
                                                        end if
                                                    end if
                                                end select
                                            end if
                                        end if
                                    end if
                                    if (len_trim(type_buf) == 0) type_buf = 'real'
                                    if (var_count < MAX_VARS) then
                                        var_count = var_count + 1
                                        var_names(var_count) = ""
                                        var_types(var_count) = ""
                                        var_names(var_count) = name_buf
                                        var_types(var_count) = trim(type_buf)
                                    end if
                                end if
                            end if
                        end select
                    end if
                end if

                if (stmt%value_index > 0 .and. stmt%value_index <= arena%size) then
                    if (allocated(arena%entries(stmt%value_index)%node)) then
                        select type (val => arena%entries(stmt%value_index)%node)
                        type is (call_or_subscript_node)
                            if (len_trim(val%name) > 0) then
                                type_buf = mono_type_to_string( &
                                    val%inferred_type, include_shape=.true., &
                                    fallback='real')
                                if (len_trim(type_buf) == 0 .or. &
                                    trim(type_buf) == 'real') then
                                    func_return_type = &
                                        lookup_function_return_type( &
                                        defined_func_names, defined_func_types, &
                                        defined_func_count, val%name)
                                    if (len_trim(func_return_type) > 0) then
                                        type_buf = trim(func_return_type)
                                    end if
                                end if
                                if (len_trim(type_buf) == 0) type_buf = 'real'
                                if (.not. exists_in_list(func_names, func_count, &
                                                         trim(val%name))) then
                                    if (func_count < MAX_VARS) then
                                        func_count = func_count + 1
                                        func_names(func_count) = ""
                                        func_types(func_count) = ""
                                        func_names(func_count) = trim(val%name)
                                        func_types(func_count) = trim(type_buf)
                                    end if
                                end if
                            end if
                        end select
                    end if
                end if
            end select
        end do

        if (var_count == 0 .and. func_count == 0) return

        do i = 1, var_count
            decl_code = decl_code // "    " // trim(var_types(i)) // " :: " // &
                        trim(var_names(i)) // new_line('A')
        end do

        do i = 1, func_count
            if (.not. exists_in_list(internal_funcs, internal_count, &
                                     trim(func_names(i)))) then
                decl_code = decl_code // "    " // trim(func_types(i)) // &
                            ", external :: " // trim(func_names(i)) // new_line('A')
            end if
        end do
    end function collect_program_variable_decls

    ! Helper function to check if a name exists in a list
    logical function exists_in_list(list, count, name)
        character(len=*), intent(in) :: list(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: name
        integer :: i

        exists_in_list = .false.
        do i = 1, count
            if (trim(list(i)) == trim(name)) then
                exists_in_list = .true.
                return
            end if
        end do
    end function exists_in_list

    subroutine build_function_return_type_table(arena, func_names, func_types, count)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(inout) :: func_names(:)
        character(len=*), intent(inout) :: func_types(:)
        integer, intent(out) :: count
        integer :: i
        character(len=64) :: func_name

        count = 0
        func_names = ""
        func_types = ""

        do i = 1, arena%size
            if (count >= size(func_names)) exit
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (func => arena%entries(i)%node)
            type is (function_def_node)
                if (.not. allocated(func%name)) cycle
                func_name = trim(func%name)
                if (len_trim(func_name) == 0) cycle
                if (exists_in_list(func_names, count, func_name)) cycle
                count = count + 1
                func_names(count) = func_name
                if (allocated(func%return_type)) then
                    if (len_trim(func%return_type) > 0) then
                        func_types(count) = trim(func%return_type)
                    end if
                end if
            end select
        end do
    end subroutine build_function_return_type_table

    function lookup_function_return_type(func_names, func_types, count, &
                                         func_name) result(type_name)
        character(len=*), intent(in) :: func_names(:)
        character(len=*), intent(in) :: func_types(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: func_name
        character(len=:), allocatable :: type_name
        integer :: i

        type_name = ""
        if (len_trim(func_name) == 0) return

        do i = 1, count
            if (trim(func_names(i)) == trim(func_name)) then
                if (len_trim(func_types(i)) > 0) then
                    type_name = trim(func_types(i))
                end if
                return
            end if
        end do
    end function lookup_function_return_type
end module codegen_declarations_program_mod
