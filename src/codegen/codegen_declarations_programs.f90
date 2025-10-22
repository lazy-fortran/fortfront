module codegen_declarations_programs
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node, identifier_node, literal_node, &
                              assignment_node, array_literal_node, &
                              call_or_subscript_node
    use ast_nodes_misc, only: blank_line_node, comment_node, contains_node, &
                              implicit_statement_node, use_statement_node, &
                              interface_block_node, module_procedure_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use ast_nodes_data, only: declaration_node, module_node, block_data_node
    use string_utils_mod, only: int_to_string, to_lower
    use type_string_utils, only: mono_type_to_string
    use codegen_utilities, only: generate_grouped_body, generate_grouped_body_context
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_indent, only: indent_lines
    use codegen_loop_vars_mod, only: add_loop_variable_decls
    use codegen_declarations_inference, only: collect_program_variable_decls
    implicit none
    private
    public :: generate_code_program
    public :: generate_code_module
    public :: generate_code_block_data
    public :: generate_code_interface_block
    public :: generate_code_module_procedure

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
        character(len=:), allocatable :: extra_decl_code

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
                                     non_use_count, extra_decl_code)

        call append_program_body(arena, node, code, non_use_indices, &
                                 non_use_count, extra_decl_code, &
                                 context_has_executable_before_contains)

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

    subroutine assemble_program_header(arena, node, code, non_use_indices, &
                                       non_use_count, extra_decl_code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        character(len=:), allocatable, intent(inout) :: code
        integer, allocatable, intent(out) :: non_use_indices(:)
        integer, intent(out) :: non_use_count
        character(len=:), allocatable, intent(out) :: extra_decl_code
        logical :: has_implicit
        character(len=:), allocatable :: use_statements_code
        character(len=:), allocatable :: implicit_statements_code
        character(len=:), allocatable :: interface_blocks_code
        character(len=:), allocatable :: extra_decls

        call gather_program_header_entries(arena, node, has_implicit, &
                                           use_statements_code, &
                                           implicit_statements_code, &
                                           interface_blocks_code, non_use_indices, &
                                           non_use_count)

        if (len(use_statements_code) > 0) code = code // use_statements_code

        if (len(implicit_statements_code) > 0) then
            code = code // implicit_statements_code
        else if (.not. has_implicit) then
            code = code // "    implicit none" // new_line('A')
        end if

        if (len(interface_blocks_code) > 0) code = code // interface_blocks_code

        extra_decl_code = ""
        extra_decls = collect_program_variable_decls(arena, node)
        if (len_trim(extra_decls) > 0) then
            if (.not. has_implicit) then
                code = code // extra_decls
            else
                extra_decl_code = extra_decls
            end if
        end if
    end subroutine assemble_program_header

    subroutine gather_program_header_entries(arena, node, has_implicit, &
                                             use_statements_code, &
                                             implicit_statements_code, &
                                             interface_blocks_code, non_use_indices, &
                                             non_use_count)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        logical, intent(out) :: has_implicit
        character(len=:), allocatable, intent(out) :: use_statements_code
        character(len=:), allocatable, intent(out) :: implicit_statements_code
        character(len=:), allocatable, intent(out) :: interface_blocks_code
        integer, allocatable, intent(out) :: non_use_indices(:)
        integer, intent(out) :: non_use_count
        integer :: i

        has_implicit = .false.
        use_statements_code = ""
        implicit_statements_code = ""
        interface_blocks_code = ""
        non_use_count = 0

        if (.not. allocated(node%body_indices)) then
            allocate (non_use_indices(0))
            return
        end if

        allocate (non_use_indices(size(node%body_indices)))

        do i = 1, size(node%body_indices)
            call categorize_header_entry(arena, node%body_indices(i), has_implicit, &
                                         use_statements_code, &
                                         implicit_statements_code, &
                                         interface_blocks_code, non_use_indices, &
                                         non_use_count)
        end do
    end subroutine gather_program_header_entries

    subroutine categorize_header_entry(arena, body_index, has_implicit, &
                                       use_statements_code, implicit_statements_code, &
                                       interface_blocks_code, non_use_indices, &
                                       non_use_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_index
        logical, intent(inout) :: has_implicit
        character(len=:), allocatable, intent(inout) :: use_statements_code
        character(len=:), allocatable, intent(inout) :: implicit_statements_code
        character(len=:), allocatable, intent(inout) :: interface_blocks_code
        integer, intent(inout) :: non_use_indices(:)
        integer, intent(inout) :: non_use_count
        logical :: is_header_stmt
        character(len=:), allocatable :: stmt_code
        character(len=:), allocatable :: lowered_value

        if (body_index <= 0 .or. body_index > arena%size) return
        if (.not. allocated(arena%entries(body_index)%node)) return

        is_header_stmt = .false.

        select type (ib => arena%entries(body_index)%node)
        type is (use_statement_node)
            is_header_stmt = .true.
            stmt_code = generate_code_from_arena(arena, body_index)
            use_statements_code = use_statements_code // "    " // stmt_code // &
                                  new_line('A')
        type is (comment_node)
            ! Legacy statement comments (COMMON/EQUIVALENCE) are always header statements
            if (is_legacy_statement_comment(ib) .or. non_use_count == 0) then
                is_header_stmt = .true.
                stmt_code = generate_code_from_arena(arena, body_index)
                if (is_legacy_statement_comment(ib)) then
                    ! Legacy statements go after implicit but before other declarations
                    implicit_statements_code = implicit_statements_code // "    " // &
                                              stmt_code // new_line('A')
                else
                    call append_header_trivia(stmt_code, use_statements_code, &
                                              implicit_statements_code, &
                                              interface_blocks_code)
                end if
            end if
        type is (blank_line_node)
            if (non_use_count == 0) then
                is_header_stmt = .true.
                stmt_code = generate_code_from_arena(arena, body_index)
                call append_header_trivia(stmt_code, use_statements_code, &
                                          implicit_statements_code, &
                                          interface_blocks_code)
            end if
        type is (implicit_statement_node)
            is_header_stmt = .true.
            if (ib%is_none) has_implicit = .true.
            stmt_code = generate_code_from_arena(arena, body_index)
            if (len_trim(stmt_code) > 0) then
                implicit_statements_code = implicit_statements_code // "    " // &
                                           trim(stmt_code) // new_line('A')
            end if
        type is (interface_block_node)
            is_header_stmt = .true.
            stmt_code = generate_code_from_arena(arena, body_index)
            interface_blocks_code = interface_blocks_code // &
                                    indent_lines(stmt_code, 1) // new_line('A')
        type is (literal_node)
            if (allocated(ib%value)) then
                lowered_value = to_lower(ib%value)
                if (index(lowered_value, 'implicit none') > 0) then
                    has_implicit = .true.
                    is_header_stmt = .true.
                    if (len_trim(ib%value) > 0) then
                        implicit_statements_code = implicit_statements_code // &
                                                   "    " // trim(ib%value) // &
                                                   new_line('A')
                    end if
                end if
            end if
        end select

        if (is_header_stmt) return

        non_use_count = non_use_count + 1
        if (non_use_count <= size(non_use_indices)) then
            non_use_indices(non_use_count) = body_index
        end if
    end subroutine categorize_header_entry

    subroutine append_header_trivia(fragment, use_code, implicit_code, interface_code)
        character(len=*), intent(in) :: fragment
        character(len=:), allocatable, intent(inout) :: use_code
        character(len=:), allocatable, intent(inout) :: implicit_code
        character(len=:), allocatable, intent(inout) :: interface_code
        character(len=:), allocatable :: trimmed_fragment
        logical :: is_blank

        if (len(fragment) == 0) return

        trimmed_fragment = fragment
        is_blank = len_trim(trimmed_fragment) == 0

        if (len(interface_code) > 0) then
            if (is_blank) then
                interface_code = interface_code // trimmed_fragment
            else
                interface_code = interface_code // "    " // trim(trimmed_fragment) // &
                                 new_line('A')
            end if
        else if (len(implicit_code) > 0) then
            if (is_blank) then
                implicit_code = implicit_code // trimmed_fragment
            else
                implicit_code = implicit_code // "    " // trim(trimmed_fragment) // &
                                new_line('A')
            end if
        else
            if (is_blank) then
                use_code = use_code // trimmed_fragment
            else
                use_code = use_code // "    " // trim(trimmed_fragment) // new_line('A')
            end if
        end if
    end subroutine append_header_trivia

    subroutine append_program_body(arena, node, code, non_use_indices, &
                                   non_use_count, extra_decl_code, &
                                   context_has_executable_before_contains)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: non_use_indices(:)
        integer, intent(in) :: non_use_count
        character(len=*), intent(in) :: extra_decl_code
        logical, intent(in) :: context_has_executable_before_contains
        character(len=:), allocatable :: body_code

        if (non_use_count <= 0) return

        body_code = generate_grouped_body_with_context( &
                    arena, non_use_indices(1:non_use_count), 1, &
                    context_has_executable_before_contains)

        call insert_program_decls(code, body_code, extra_decl_code)

        if (index(body_code, 'output_unit') > 0) then
            call ensure_output_unit_use(code)
        end if

        call add_loop_variable_decls(code, body_code)

        code = code // body_code
    end subroutine append_program_body

    subroutine insert_program_decls(code, body_code, extra_decl_code)
        character(len=:), allocatable, intent(inout) :: code
        character(len=:), allocatable, intent(inout) :: body_code
        character(len=*), intent(in) :: extra_decl_code
        integer :: impl_pos, insert_pos
        character(len=:), allocatable :: before_code, after_code

        if (len_trim(extra_decl_code) == 0) return
        if (len(body_code) == 0) then
            code = code // extra_decl_code
            return
        end if

        impl_pos = index(body_code, 'implicit none')
        if (impl_pos > 0) then
            insert_pos = impl_pos + len('implicit none')
            do while (insert_pos <= len(body_code))
                if (body_code(insert_pos:insert_pos) == new_line('A')) then
                    insert_pos = insert_pos + 1
                    exit
                end if
                insert_pos = insert_pos + 1
            end do

            if (insert_pos <= len(body_code)) then
                before_code = body_code(1:insert_pos - 1)
                after_code = body_code(insert_pos:)
            else
                before_code = body_code
                after_code = ''
            end if
            body_code = before_code // extra_decl_code // after_code
        else
            code = code // extra_decl_code
        end if
    end subroutine insert_program_decls

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
                    if (len(trivia_code) > 0) trivia_code = trivia_code // &
                                                            new_line('A')
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

    ! Collect variable declarations for undeclared identifiers in programs

    function generate_code_module(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code

        code = build_module_header(arena, node)
        code = code // collect_module_declarations(arena, node)
        code = code // build_contains_section(arena, node)
        code = code // "end module " // node%name
    end function generate_code_module

    function generate_code_block_data(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(block_data_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code
        integer :: i

        code = "block data"
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
        end if
        code = code // new_line('A')

        if (allocated(node%statement_indices)) then
            do i = 1, size(node%statement_indices)
                body_code = generate_code_from_arena(arena, node%statement_indices(i))
                if (len(body_code) > 0) then
                    code = code // "    " // body_code // new_line('A')
                end if
            end do
        end if

        code = code // "end block data"
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
        end if
    end function generate_code_block_data

    function build_module_header(arena, node) result(header)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        character(len=:), allocatable :: header

        header = "module " // node%name // new_line('A')
        if (.not. module_has_implicit_none(arena, node)) then
            header = header // "    implicit none" // new_line('A')
        end if
    end function build_module_header

    logical function module_has_implicit_none(arena, node) result(has_implicit)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        integer :: i
        integer :: decl_index

        has_implicit = .false.
        if (.not. allocated(node%declaration_indices)) return

        do i = 1, size(node%declaration_indices)
            decl_index = node%declaration_indices(i)
            if (decl_index <= 0 .or. decl_index > arena%size) cycle
            if (.not. allocated(arena%entries(decl_index)%node)) cycle

            select type (decl => arena%entries(decl_index)%node)
            type is (implicit_statement_node)
                if (decl%is_none) then
                    has_implicit = .true.
                    return
                end if
            type is (literal_node)
                if (allocated(decl%value)) then
                    if (index(decl%value, "implicit none") > 0) then
                        has_implicit = .true.
                        return
                    end if
                end if
            end select
        end do
    end function module_has_implicit_none

    function collect_module_declarations(arena, node) result(body_code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        character(len=:), allocatable :: body_code

        if (.not. allocated(node%declaration_indices)) then
            body_code = ""
            return
        end if

        body_code = generate_grouped_body(arena, node%declaration_indices, 1)
    end function collect_module_declarations

    function build_contains_section(arena, node) result(section_code)
        type(ast_arena_t), intent(in) :: arena
        type(module_node), intent(in) :: node
        character(len=:), allocatable :: section_code
        character(len=:), allocatable :: procedure_code
        integer :: i
        logical :: has_entries
        logical :: has_more

        section_code = ""
        has_entries = .false.

        if (.not. node%has_contains) return
        if (.not. allocated(node%procedure_indices)) return

        section_code = "contains" // new_line('A')

        do i = 1, size(node%procedure_indices)
            procedure_code = collect_contained_procedure(arena, &
                                                         node%procedure_indices(i))
            if (len(procedure_code) == 0) cycle
            has_entries = .true.
            has_more = i < size(node%procedure_indices)
            section_code = section_code // format_contained_procedure( &
                           procedure_code, has_more)
        end do

        if (.not. has_entries) section_code = ""
    end function build_contains_section

    function collect_contained_procedure(arena, procedure_index) result(proc_code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: procedure_index
        character(len=:), allocatable :: proc_code

        proc_code = ""
        if (procedure_index <= 0 .or. procedure_index > arena%size) return
        if (.not. allocated(arena%entries(procedure_index)%node)) return

        proc_code = generate_code_from_arena(arena, procedure_index)
    end function collect_contained_procedure

    function format_contained_procedure(proc_code, has_more) result(formatted)
        character(len=*), intent(in) :: proc_code
        logical, intent(in) :: has_more
        character(len=:), allocatable :: formatted

        formatted = "    " // proc_code
        if (has_more) then
            formatted = formatted // new_line('A') // new_line('A')
        else
            formatted = formatted // new_line('A')
        end if
    end function format_contained_procedure

    function generate_code_interface_block(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(interface_block_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        character(len=:), allocatable :: body_code

        if (node%is_abstract) then
            code = "abstract interface"
        else
            code = "interface"
        end if
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
        end if
        code = code // new_line('A')

        if (allocated(node%procedure_indices)) then
            body_code = generate_grouped_body(arena, node%procedure_indices, 1)
            if (len(body_code) > 0) code = code // body_code
        end if

        code = code // "end interface"
        if (allocated(node%name)) then
            if (len_trim(node%name) > 0) code = code // " " // trim(node%name)
        end if
    end function generate_code_interface_block

    function generate_code_module_procedure(node) result(code)
        type(module_procedure_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i
        character(len=:), allocatable :: name_text
        logical :: first_name

        code = "module procedure"
        first_name = .true.
        if (allocated(node%procedure_names)) then
            do i = 1, size(node%procedure_names)
                if (.not. allocated(node%procedure_names(i)%s)) cycle
                name_text = trim(node%procedure_names(i)%s)
                if (len_trim(name_text) == 0) cycle
                if (first_name) then
                    code = code // " " // name_text
                    first_name = .false.
                else
                    code = code // ", " // name_text
                end if
            end do
        end if
    end function generate_code_module_procedure
end module codegen_declarations_programs
