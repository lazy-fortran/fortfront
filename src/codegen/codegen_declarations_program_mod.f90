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

    integer, parameter :: program_decl_max_vars = 256

    type :: program_decl_state_t
        character(len=64) :: declared_names(program_decl_max_vars)
        character(len=64) :: var_names(program_decl_max_vars)
        character(len=64) :: var_types(program_decl_max_vars)
        character(len=64) :: func_names(program_decl_max_vars)
        character(len=64) :: func_types(program_decl_max_vars)
        character(len=64) :: internal_funcs(program_decl_max_vars)
        character(len=64) :: defined_func_names(program_decl_max_vars)
        character(len=64) :: defined_func_types(program_decl_max_vars)
        integer :: declared_count
        integer :: var_count
        integer :: func_count
        integer :: internal_count
        integer :: defined_func_count
    end type program_decl_state_t

contains

    ! Generate code for program nodes
    function generate_code_program(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        integer, allocatable :: non_use_indices(:)
        integer :: non_use_count
        logical :: context_has_executable_before_contains

        context_has_executable_before_contains = &
            has_executable_before_contains(arena, node)

        if (node%name == "__MULTI_UNIT__") then
            code = generate_multi_unit_program(arena, node)
            return
        end if

        if (program_is_trivial_wrapper(arena, node_index, node%name)) then
            code = collect_trivial_program_trivia(arena, node_index)
            return
        end if

        code = "program " // node%name // new_line('A')

        call assemble_program_header(arena, node, code, non_use_indices, &
                                     non_use_count)

        call append_program_body(arena, node, code, non_use_indices, &
                                 non_use_count, context_has_executable_before_contains)

        if (allocated(non_use_indices)) then
            deallocate (non_use_indices)
        end if

        code = code // "end program " // node%name
    end function generate_code_program

    logical function has_executable_before_contains(arena, node) result(has_exec)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        logical :: has_non_trivial_body
        logical :: found_contains
        integer :: i

        has_non_trivial_body = .false.
        found_contains = .false.
        has_exec = .false.

        if (.not. allocated(node%body_indices)) return

        do i = 1, size(node%body_indices)
            if (node%body_indices(i) <= 0 .or. node%body_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(node%body_indices(i))%node)) cycle
            select type (body_node => arena%entries(node%body_indices(i))%node)
            type is (contains_node)
                found_contains = .true.
                exit
            type is (comment_node)
            type is (blank_line_node)
            class default
                has_non_trivial_body = .true.
            end select
        end do

        has_exec = has_non_trivial_body .and. found_contains
    end function has_executable_before_contains

    function generate_multi_unit_program(arena, node) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i, child_index

        code = ""
        if (.not. allocated(node%body_indices)) return

        do i = 1, size(node%body_indices)
            child_index = node%body_indices(i)
            if (child_index <= 0 .or. child_index > arena%size) cycle
            if (.not. allocated(arena%entries(child_index)%node)) cycle

            select type (child => arena%entries(child_index)%node)
            type is (program_node)
                if (append_trivial_program(arena, code, child_index, child%name)) cycle
            type is (subroutine_def_node)
                if (skip_duplicate_empty_subroutine(arena, node, child, i)) cycle
            end select

            if (len(code) > 0) code = code // new_line('A') // new_line('A')
            code = code // generate_code_from_arena(arena, child_index)
        end do
    end function generate_multi_unit_program

    logical function append_trivial_program(arena, code, program_index, name)
        type(ast_arena_t), intent(in) :: arena
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: program_index
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: snippet

        snippet = gather_trivial_program_trivia(arena, program_index, name)
        if (len_trim(snippet) == 0) then
            append_trivial_program = .false.
            return
        end if

        if (len(code) > 0) code = code // new_line('A') // new_line('A')
        code = code // snippet
        append_trivial_program = .true.
    end function append_trivial_program

    function gather_trivial_program_trivia(arena, body_index, name) result(snippet)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: snippet

        snippet = ""
        if (.not. program_is_trivial_wrapper(arena, body_index, name)) return

        snippet = collect_trivial_program_trivia(arena, body_index)
    end function gather_trivial_program_trivia

    logical function skip_duplicate_empty_subroutine(arena, node, child, position) &
        result(skip)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        type(subroutine_def_node), intent(in) :: child
        integer, intent(in) :: position

        if (subroutine_has_body_or_params(child)) then
            skip = .false.
        else
            skip = has_prior_subroutine_with_name(arena, node, child%name, position)
        end if
    end function skip_duplicate_empty_subroutine

    logical function subroutine_has_body_or_params(child) result(has_entries)
        type(subroutine_def_node), intent(in) :: child

        has_entries = .false.
        if (allocated(child%body_indices)) then
            if (size(child%body_indices) > 0) then
                has_entries = .true.
                return
            end if
        end if

        if (allocated(child%param_indices)) then
            has_entries = size(child%param_indices) > 0
        end if
    end function subroutine_has_body_or_params

    logical function has_prior_subroutine_with_name(arena, node, name, position) &
        result(found)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        character(len=*), intent(in) :: name
        integer, intent(in) :: position
        integer :: j, idx

        found = .false.
        if (.not. allocated(node%body_indices)) return

        do j = 1, position - 1
            idx = node%body_indices(j)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (prev => arena%entries(idx)%node)
            type is (subroutine_def_node)
                if (prev%name == name) then
                    found = .true.
                    return
                end if
            end select
        end do
    end function has_prior_subroutine_with_name

    subroutine assemble_program_header(arena, node, code, non_use_indices, non_use_count)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        character(len=:), allocatable, intent(inout) :: code
        integer, allocatable, intent(out) :: non_use_indices(:)
        integer, intent(out) :: non_use_count
        logical :: has_implicit
        character(len=:), allocatable :: use_statements_code
        character(len=:), allocatable :: extra_decls

        call gather_program_header_entries(arena, node, has_implicit, &
                                           use_statements_code, non_use_indices, &
                                           non_use_count)

        if (len(use_statements_code) > 0) code = code // use_statements_code

        if (.not. has_implicit) then
            code = code // "    implicit none" // new_line('A')
        end if

        extra_decls = collect_program_variable_decls(arena, node)
        if (len_trim(extra_decls) > 0) code = code // extra_decls
    end subroutine assemble_program_header

    subroutine gather_program_header_entries(arena, node, has_implicit, &
                                             use_statements_code, non_use_indices, &
                                             non_use_count)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        logical, intent(out) :: has_implicit
        character(len=:), allocatable, intent(out) :: use_statements_code
        integer, allocatable, intent(out) :: non_use_indices(:)
        integer, intent(out) :: non_use_count
        integer :: i

        has_implicit = .false.
        use_statements_code = ""
        non_use_count = 0

        if (.not. allocated(node%body_indices)) then
            allocate (non_use_indices(0))
            return
        end if

        allocate (non_use_indices(size(node%body_indices)))

        do i = 1, size(node%body_indices)
            call categorize_header_entry(arena, node%body_indices(i), has_implicit, &
                                         use_statements_code, non_use_indices, &
                                         non_use_count)
        end do
    end subroutine gather_program_header_entries

    subroutine categorize_header_entry(arena, body_index, has_implicit, &
                                       use_statements_code, non_use_indices, &
                                       non_use_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        logical, intent(inout) :: has_implicit
        character(len=:), allocatable, intent(inout) :: use_statements_code
        integer, intent(inout) :: non_use_indices(:)
        integer, intent(inout) :: non_use_count
        logical :: is_use_stmt
        character(len=:), allocatable :: use_code

        if (body_index <= 0 .or. body_index > arena%size) return
        if (.not. allocated(arena%entries(body_index)%node)) return

        is_use_stmt = .false.

        select type (ib => arena%entries(body_index)%node)
        type is (use_statement_node)
            is_use_stmt = .true.
            use_code = generate_code_from_arena(arena, body_index)
            use_statements_code = use_statements_code // "    " // use_code // &
                                  new_line('A')
        type is (implicit_statement_node)
            if (ib%is_none) has_implicit = .true.
        type is (literal_node)
            if (allocated(ib%value)) then
                if (index(ib%value, 'implicit none') > 0) has_implicit = .true.
            end if
        end select

        if (is_use_stmt) return

        non_use_count = non_use_count + 1
        if (non_use_count <= size(non_use_indices)) then
            non_use_indices(non_use_count) = body_index
        end if
    end subroutine categorize_header_entry

    subroutine append_program_body(arena, node, code, non_use_indices, non_use_count, &
                                   context_has_executable_before_contains)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: non_use_indices(:)
        integer, intent(in) :: non_use_count
        logical, intent(in) :: context_has_executable_before_contains
        character(len=:), allocatable :: body_code

        if (non_use_count <= 0) return

        body_code = generate_grouped_body_with_context( &
                    arena, non_use_indices(1:non_use_count), 1, &
                    context_has_executable_before_contains)

        if (index(body_code, 'output_unit') > 0) then
            call ensure_output_unit_use(code)
        end if

        call add_loop_variable_decls(code, body_code)

        code = code // body_code
    end subroutine append_program_body

    subroutine ensure_output_unit_use(code)
        character(len=:), allocatable, intent(inout) :: code
        logical :: has_iso_line

        has_iso_line = try_augment_existing_iso_use(code)
        if (.not. has_iso_line) call insert_new_iso_use(code)
    end subroutine ensure_output_unit_use

    logical function try_augment_existing_iso_use(code) result(found_iso)
        character(len=:), allocatable, intent(inout) :: code
        integer :: search_pos, iso_pos

        found_iso = .false.
        search_pos = 1

        do
            iso_pos = index(code(search_pos:), 'iso_fortran_env')
            if (iso_pos == 0) exit
            iso_pos = search_pos + iso_pos - 1

            call augment_iso_line_at_position(code, iso_pos, search_pos, found_iso)
            if (found_iso) exit

            if (search_pos <= 0 .or. search_pos > len(code)) exit
        end do
    end function try_augment_existing_iso_use

    subroutine augment_iso_line_at_position(code, iso_pos, search_pos, found_iso)
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: iso_pos
        integer, intent(inout) :: search_pos
        logical, intent(inout) :: found_iso
        integer :: line_start, line_end
        logical :: iso_has_only, iso_has_output
        character(len=:), allocatable :: iso_line

        call find_line_bounds(code, iso_pos, line_start, line_end)
        found_iso = .true.

        if (line_end > len(code)) then
            iso_line = code(line_start:)
        else
            iso_line = code(line_start:line_end - 1)
        end if

        iso_has_only = index(to_lower(iso_line), 'only:') > 0
        iso_has_output = index(to_lower(iso_line), 'output_unit') > 0

        if (iso_has_only .and. .not. iso_has_output) then
            call inject_output_unit_into_line(code, line_start, line_end, iso_line)
            iso_has_output = .true.
        end if

        if (.not. iso_has_only .or. iso_has_output) then
            search_pos = -1
        else if (line_end <= len(code)) then
            search_pos = line_end + 1
        else
            search_pos = -1
        end if
    end subroutine augment_iso_line_at_position

    subroutine find_line_bounds(code, position, line_start, line_end)
        character(len=*), intent(in) :: code
        integer, intent(in) :: position
        integer, intent(out) :: line_start, line_end

        line_start = position
        do while (line_start > 1 .and. code(line_start - 1:line_start - 1) /= &
                  new_line('A'))
            line_start = line_start - 1
        end do

        line_end = position
        do while (line_end <= len(code) .and. code(line_end:line_end) /= &
                  new_line('A'))
            line_end = line_end + 1
        end do
    end subroutine find_line_bounds

    subroutine inject_output_unit_into_line(code, line_start, line_end, iso_line)
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: line_start, line_end
        character(len=*), intent(in) :: iso_line
        character(len=:), allocatable :: prefix, suffix, modified_line

        call split_code_at_line(code, line_start, line_end, prefix, suffix)
        modified_line = append_output_unit_to_iso_line(iso_line)
        code = prefix // modified_line // new_line('A') // suffix
    end subroutine inject_output_unit_into_line

    subroutine split_code_at_line(code, line_start, line_end, prefix, suffix)
        character(len=*), intent(in) :: code
        integer, intent(in) :: line_start, line_end
        character(len=:), allocatable, intent(out) :: prefix, suffix

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
    end subroutine split_code_at_line

    function append_output_unit_to_iso_line(iso_line) result(modified_line)
        character(len=*), intent(in) :: iso_line
        character(len=:), allocatable :: modified_line
        integer :: comment_pos
        character(len=:), allocatable :: trimmed_line, iso_comment

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

        modified_line = trimmed_line // ', output_unit'
        if (len_trim(iso_comment) > 0) then
            modified_line = modified_line // ' ' // iso_comment
        end if
    end function append_output_unit_to_iso_line

    subroutine insert_new_iso_use(code)
        character(len=:), allocatable, intent(inout) :: code
        integer :: header_end
        character(len=:), allocatable :: prefix, suffix

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
    end subroutine insert_new_iso_use

    subroutine add_loop_variable_decls(code, body_code)
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable, intent(inout) :: body_code
        character(len=:), allocatable :: loop_vars(:)
        integer :: n_vars

        if (len(body_code) == 0) return

        allocate (character(len=32) :: loop_vars(20))
        loop_vars = ""
        n_vars = 0

        call scan_for_loop_variables(body_code, loop_vars, n_vars)
        call inject_loop_var_declarations(code, body_code, loop_vars, n_vars)

        deallocate (loop_vars)
    end subroutine add_loop_variable_decls

    subroutine scan_for_loop_variables(body_code, loop_vars, n_vars)
        character(len=*), intent(in) :: body_code
        character(len=*), intent(inout) :: loop_vars(:)
        integer, intent(inout) :: n_vars
        integer :: pos, start_pos, end_pos

        pos = 1
        do while (pos <= len(body_code))
            start_pos = index(body_code(pos:), "= (/(")
            if (start_pos == 0) start_pos = index(body_code(pos:), "= (/ (")
            if (start_pos == 0) then
                start_pos = index(body_code(pos:), "= [(")
                if (start_pos > 0) then
                    start_pos = pos + start_pos - 1
                    end_pos = index(body_code(start_pos:), ")]")
                    if (end_pos > 0) then
                        end_pos = start_pos + end_pos - 1
                        call extract_loop_vars_from_section( &
                            body_code(start_pos:end_pos), loop_vars, n_vars)
                    end if
                    pos = start_pos + 3
                else
                    exit
                end if
            else
                start_pos = pos + start_pos - 1
                end_pos = index(body_code(start_pos:), "/)")
                if (end_pos > 0) then
                    end_pos = start_pos + end_pos - 1
                    call extract_loop_vars_from_section( &
                        body_code(start_pos:end_pos), loop_vars, n_vars)
                end if
                pos = start_pos + 5
            end if
        end do
    end subroutine scan_for_loop_variables

    subroutine inject_loop_var_declarations(code, body_code, loop_vars, n_vars)
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable, intent(inout) :: body_code
        character(len=*), intent(in) :: loop_vars(:)
        integer, intent(in) :: n_vars
        integer :: impl_pos

        if (n_vars == 0 .and. .not. (index(body_code, "[(") > 0 .and. &
                                     index(body_code, ")]") > 0)) return

        impl_pos = index(body_code, "implicit none")
        if (impl_pos > 0) then
            call inject_decls_after_implicit(body_code, impl_pos, loop_vars, n_vars)
        else
            call inject_decls_before_body(code, body_code, loop_vars, n_vars)
        end if
    end subroutine inject_loop_var_declarations

    subroutine inject_decls_after_implicit(body_code, impl_pos, loop_vars, n_vars)
        character(len=:), allocatable, intent(inout) :: body_code
        integer, intent(in) :: impl_pos
        character(len=*), intent(in) :: loop_vars(:)
        integer, intent(in) :: n_vars
        integer :: insert_pos, i
        character(len=:), allocatable :: before_code, after_code, name_buf
        logical :: already_declared

        insert_pos = impl_pos + 13
        do while (insert_pos <= len(body_code))
            if (body_code(insert_pos:insert_pos) == new_line('A')) then
                insert_pos = insert_pos + 1
                exit
            end if
            insert_pos = insert_pos + 1
        end do

        before_code = body_code(1:insert_pos - 1)
        after_code = body_code(insert_pos:)

        if (n_vars > 0) then
            do i = 1, n_vars
                name_buf = trim(loop_vars(i))
                already_declared = index(body_code, "integer :: "//name_buf) > 0
                if (.not. already_declared) then
                    before_code = before_code // "    integer :: " // &
                                  name_buf // new_line('A')
                end if
            end do
        else
            call add_default_loop_var_if_needed(body_code, before_code)
        end if

        body_code = before_code // after_code
    end subroutine inject_decls_after_implicit

    subroutine inject_decls_before_body(code, body_code, loop_vars, n_vars)
        character(len=:), allocatable, intent(inout) :: code
        character(len=*), intent(in) :: body_code
        character(len=*), intent(in) :: loop_vars(:)
        integer, intent(in) :: n_vars
        integer :: i
        character(len=:), allocatable :: name_buf
        logical :: already_declared

        if (n_vars > 0) then
            do i = 1, n_vars
                name_buf = trim(loop_vars(i))
                already_declared = index(body_code, "integer :: "//name_buf) > 0
                if (.not. already_declared) then
                    already_declared = index(code, "integer :: "//name_buf) > 0
                end if
                if (.not. already_declared) then
                    code = code // "    integer :: " // name_buf // new_line('A')
                end if
            end do
        else
            call add_default_loop_var_to_code(code, body_code)
        end if
    end subroutine inject_decls_before_body

    subroutine add_default_loop_var_if_needed(body_code, before_code)
        character(len=*), intent(in) :: body_code
        character(len=:), allocatable, intent(inout) :: before_code

        if (index(body_code, "[(") > 0 .and. index(body_code, ")]") > 0) then
            if (index(body_code, "integer :: i") == 0) then
                before_code = before_code // "    integer :: i" // new_line('A')
            end if
        end if
    end subroutine add_default_loop_var_if_needed

    subroutine add_default_loop_var_to_code(code, body_code)
        character(len=:), allocatable, intent(inout) :: code
        character(len=*), intent(in) :: body_code

        if (index(body_code, "[(") > 0 .and. index(body_code, ")]") > 0) then
            if (index(body_code, "integer :: i") == 0 .and. &
                index(code, "integer :: i") == 0) then
                code = code // "    integer :: i" // new_line('A')
            end if
        end if
    end subroutine add_default_loop_var_to_code

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
        integer :: pos, eq_pos

        pos = 1
        do
            eq_pos = find_next_equal(section, pos)
            if (eq_pos <= 0) exit
            call try_register_loop_var(section, eq_pos, loop_vars, n_vars)
            pos = eq_pos + 1
        end do
    end subroutine extract_loop_vars_from_section

    integer function find_next_equal(section, start_pos) result(position)
        character(len=*), intent(in) :: section
        integer, intent(in) :: start_pos
        integer :: local_pos

        position = 0
        if (start_pos < 1 .or. start_pos > len_trim(section)) return

        local_pos = index(section(start_pos:), "=")
        if (local_pos == 0) return

        position = start_pos + local_pos - 1
    end function find_next_equal

    subroutine try_register_loop_var(section, eq_pos, loop_vars, n_vars)
        character(len=*), intent(in) :: section
        integer, intent(in) :: eq_pos
        character(len=*), intent(inout) :: loop_vars(:)
        integer, intent(inout) :: n_vars
        character(len=:), allocatable :: var_name

        var_name = extract_loop_var_name(section, eq_pos)
        if (len_trim(var_name) == 0) return
        if (len_trim(var_name) > 8) return
        if (.not. has_loop_range_after(section, eq_pos)) return

        call add_loop_variable(loop_vars, n_vars, var_name)
    end subroutine try_register_loop_var

    function extract_loop_var_name(section, eq_pos) result(var_name)
        character(len=*), intent(in) :: section
        integer, intent(in) :: eq_pos
        character(len=:), allocatable :: var_name
        integer :: i, start_pos

        var_name = ""
        if (eq_pos <= 1) return

        start_pos = eq_pos - 1
        do i = start_pos, 1, -1
            if (section(i:i) == ' ' .or. section(i:i) == ',' .or. &
                section(i:i) == '(') then
                exit
            end if
            start_pos = i - 1
        end do

        var_name = adjustl(trim(section(start_pos + 1:eq_pos - 1)))
    end function extract_loop_var_name

    logical function has_loop_range_after(section, eq_pos) result(has_range)
        character(len=*), intent(in) :: section
        integer, intent(in) :: eq_pos

        has_range = index(section(eq_pos + 1:), ",") > 0
    end function has_loop_range_after

    subroutine add_loop_variable(loop_vars, n_vars, var_name)
        character(len=*), intent(inout) :: loop_vars(:)
        integer, intent(inout) :: n_vars
        character(len=*), intent(in) :: var_name
        integer :: i

        do i = 1, n_vars
            if (trim(loop_vars(i)) == trim(var_name)) return
        end do

        if (n_vars >= size(loop_vars)) return

        n_vars = n_vars + 1
        loop_vars(n_vars) = trim(var_name)
    end subroutine add_loop_variable

    ! Collect variable declarations for undeclared identifiers in programs
    function collect_program_variable_decls(arena, prog) result(decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        character(len=:), allocatable :: decl_code
        type(program_decl_state_t) :: state

        decl_code = ""
        if (.not. allocated(prog%body_indices)) return

        call initialize_program_decl_state(state)
        call populate_defined_function_table(arena, state)
        call collect_declared_symbols(arena, prog, state)
        call collect_assignment_symbols(arena, prog, state)

        if (state%var_count == 0 .and. state%func_count == 0) return

        decl_code = emit_program_declarations(state)
    end function collect_program_variable_decls

    subroutine initialize_program_decl_state(state)
        type(program_decl_state_t), intent(out) :: state

        state%declared_names = ""
        state%var_names = ""
        state%var_types = ""
        state%func_names = ""
        state%func_types = ""
        state%internal_funcs = ""
        state%defined_func_names = ""
        state%defined_func_types = ""
        state%declared_count = 0
        state%var_count = 0
        state%func_count = 0
        state%internal_count = 0
        state%defined_func_count = 0
    end subroutine initialize_program_decl_state

    subroutine populate_defined_function_table(arena, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_decl_state_t), intent(inout) :: state

        call build_function_return_type_table(arena, state%defined_func_names, &
                                              state%defined_func_types, &
                                              state%defined_func_count)
    end subroutine populate_defined_function_table

    subroutine collect_declared_symbols(arena, prog, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, j, idx

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (decl => arena%entries(idx)%node)
            type is (declaration_node)
                if (decl%is_multi_declaration .and. allocated(decl%var_names)) then
                    do j = 1, size(decl%var_names)
                        call record_declared_name(state, trim(decl%var_names(j)))
                    end do
                else
                    call record_declared_name(state, trim(decl%var_name))
                end if
            type is (function_def_node)
                call try_add_internal_function(state, trim(decl%name))
            end select
        end do
    end subroutine collect_declared_symbols

    subroutine record_declared_name(state, name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name

        if (len_trim(name) == 0) return
        if (state%declared_count >= program_decl_max_vars) return
        state%declared_count = state%declared_count + 1
        state%declared_names(state%declared_count) = name
    end subroutine record_declared_name

    subroutine try_add_internal_function(state, name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name

        if (len_trim(name) == 0) return
        if (state%internal_count >= program_decl_max_vars) return
        if (exists_in_list(state%internal_funcs, state%internal_count, name)) return
        state%internal_count = state%internal_count + 1
        state%internal_funcs(state%internal_count) = name
    end subroutine try_add_internal_function

    subroutine collect_assignment_symbols(arena, prog, state)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: prog
        type(program_decl_state_t), intent(inout) :: state
        integer :: i, idx

        do i = 1, size(prog%body_indices)
            idx = prog%body_indices(i)
            if (idx <= 0 .or. idx > arena%size) cycle
            if (.not. allocated(arena%entries(idx)%node)) cycle
            select type (stmt => arena%entries(idx)%node)
            type is (assignment_node)
                call process_assignment_target(arena, stmt, state)
                call process_assignment_value(arena, stmt, state)
            end select
        end do
    end subroutine collect_assignment_symbols

    subroutine process_assignment_target(arena, stmt, state)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        type(program_decl_state_t), intent(inout) :: state
        integer :: target_idx
        character(len=:), allocatable :: name_buf
        character(len=:), allocatable :: type_buf
        character(len=:), allocatable :: func_return_type

        target_idx = stmt%target_index
        if (target_idx <= 0 .or. target_idx > arena%size) return
        if (.not. allocated(arena%entries(target_idx)%node)) return

        select type (id => arena%entries(target_idx)%node)
        type is (identifier_node)
            name_buf = trim(id%name)
            if (len_trim(name_buf) == 0) return
            if (exists_in_list(state%declared_names, state%declared_count, &
                               name_buf)) return
            if (exists_in_list(state%var_names, state%var_count, name_buf)) return

            type_buf = mono_type_to_string(id%inferred_type, include_shape=.true., &
                                           fallback='real')
            if (len_trim(type_buf) == 0 .or. trim(type_buf) == 'real') then
                func_return_type = infer_function_return_type_from_rhs(arena, &
                                                                       stmt, state)
                if (len_trim(func_return_type) > 0) type_buf = trim(func_return_type)
            end if
            if (len_trim(type_buf) == 0) type_buf = 'real'

            call try_add_variable(state, name_buf, trim(type_buf))
        end select
    end subroutine process_assignment_target

    function infer_function_return_type_from_rhs(arena, stmt, state) result(type_name)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        type(program_decl_state_t), intent(in) :: state
        character(len=:), allocatable :: type_name
        integer :: value_idx

        type_name = ""
        value_idx = stmt%value_index
        if (value_idx <= 0 .or. value_idx > arena%size) return
        if (.not. allocated(arena%entries(value_idx)%node)) return

        select type (rhs => arena%entries(value_idx)%node)
        type is (call_or_subscript_node)
            if (len_trim(rhs%name) == 0) return
            type_name = lookup_function_return_type(state%defined_func_names, &
                                                    state%defined_func_types, &
                                                    state%defined_func_count, rhs%name)
        end select
    end function infer_function_return_type_from_rhs

    subroutine process_assignment_value(arena, stmt, state)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        type(program_decl_state_t), intent(inout) :: state
        integer :: value_idx
        character(len=:), allocatable :: type_buf
        character(len=:), allocatable :: func_return_type
        character(len=:), allocatable :: name_buf

        value_idx = stmt%value_index
        if (value_idx <= 0 .or. value_idx > arena%size) return
        if (.not. allocated(arena%entries(value_idx)%node)) return

        select type (val => arena%entries(value_idx)%node)
        type is (call_or_subscript_node)
            name_buf = trim(val%name)
            if (len_trim(name_buf) == 0) return
            type_buf = mono_type_to_string(val%inferred_type, include_shape=.true., &
                                           fallback='real')
            if (len_trim(type_buf) == 0 .or. trim(type_buf) == 'real') then
                func_return_type = &
                    lookup_function_return_type(state%defined_func_names, &
                                                state%defined_func_types, &
                                                state%defined_func_count, &
                                                name_buf)
                if (len_trim(func_return_type) > 0) type_buf = trim(func_return_type)
            end if
            if (len_trim(type_buf) == 0) type_buf = 'real'
            call try_add_function_reference(state, name_buf, trim(type_buf))
        end select
    end subroutine process_assignment_value

    subroutine try_add_variable(state, name, type_name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: type_name

        if (len_trim(name) == 0) return
        if (state%var_count >= program_decl_max_vars) return
        if (exists_in_list(state%var_names, state%var_count, name)) return
        state%var_count = state%var_count + 1
        state%var_names(state%var_count) = name
        state%var_types(state%var_count) = type_name
    end subroutine try_add_variable

    subroutine try_add_function_reference(state, name, type_name)
        type(program_decl_state_t), intent(inout) :: state
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: type_name

        if (len_trim(name) == 0) return
        if (state%func_count >= program_decl_max_vars) return
        if (exists_in_list(state%func_names, state%func_count, name)) return
        state%func_count = state%func_count + 1
        state%func_names(state%func_count) = name
        state%func_types(state%func_count) = type_name
    end subroutine try_add_function_reference

    function emit_program_declarations(state) result(code)
        type(program_decl_state_t), intent(in) :: state
        character(len=:), allocatable :: code
        integer :: i

        code = ""
        do i = 1, state%var_count
            code = code // "    " // trim(state%var_types(i)) // " :: " // &
                   trim(state%var_names(i)) // new_line('A')
        end do

        do i = 1, state%func_count
            if (exists_in_list(state%internal_funcs, state%internal_count, &
                               trim(state%func_names(i)))) cycle
            code = code // "    " // trim(state%func_types(i)) // &
                   ", external :: " // trim(state%func_names(i)) // new_line('A')
        end do
    end function emit_program_declarations

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
