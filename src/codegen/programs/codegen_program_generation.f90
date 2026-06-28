module codegen_program_generation
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_misc, only: blank_line_node, comment_node, contains_node, &
        directive_node, end_statement_node, &
        implicit_statement_node, interface_block_node, &
        statement_function_node
    use ast_nodes_procedure, only: subroutine_def_node, function_def_node
    use ast_nodes_data, only: module_node, multi_unit_container_node
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_program_body, only: append_program_body
    use codegen_program_header, only: assemble_program_header

    implicit none
    private
    public :: generate_code_program
    public :: generate_multi_unit_program

contains

    recursive function generate_code_program(arena, node, node_index) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        integer, intent(in) :: node_index
        character(len=:), allocatable :: code
        integer, allocatable :: non_use_indices(:)
        integer :: non_use_count
        character(len=:), allocatable :: extra_decl_code

        if (program_is_trivial_wrapper(arena, node_index, node%name)) then
            code = collect_trivial_program_trivia(arena, node_index)
            return
        end if

        code = "program "//node%name//new_line('A')

        call assemble_program_header(arena, node, code, non_use_indices, &
            non_use_count, extra_decl_code)

        call append_program_body(arena, node, code, non_use_indices, &
            non_use_count, extra_decl_code)

        if (allocated(non_use_indices)) then
            deallocate (non_use_indices)
        end if

        code = code//"end program "//node%name
    end function generate_code_program

    logical function program_contains_only_interfaces(arena, prog_index) &
            result(has_only_interfaces)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        integer :: j, child_idx
        logical :: found_interface

        has_only_interfaces = .false.
        found_interface = .false.

        if (.not. arena%has_node_at(prog_index)) return

        select type (prog => arena%entries(prog_index)%node)
            type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            has_only_interfaces = .true.
            do j = 1, size(prog%body_indices)
                child_idx = prog%body_indices(j)
                if (.not. arena%has_node_at(child_idx)) cycle
                select type (body => arena%entries(child_idx)%node)
                    type is (interface_block_node)
                    found_interface = .true.
                    type is (comment_node)
                    type is (directive_node)
                    type is (blank_line_node)
                    type is (implicit_statement_node)
                    type is (end_statement_node)
                    cycle
                class default
                    has_only_interfaces = .false.
                    return
                end select
            end do
            has_only_interfaces = has_only_interfaces .and. found_interface
        class default
            has_only_interfaces = .false.
        end select
    end function program_contains_only_interfaces

    function emit_interface_only_program(arena, prog_index) result(snippet)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=:), allocatable :: snippet
        integer :: j, child_idx
        character(len=:), allocatable :: statement_code

        snippet = ""
        if (.not. program_contains_only_interfaces(arena, prog_index)) return

        select type (prog => arena%entries(prog_index)%node)
            type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do j = 1, size(prog%body_indices)
                child_idx = prog%body_indices(j)
                if (.not. arena%has_node_at(child_idx)) cycle
                select type (body => arena%entries(child_idx)%node)
                    type is (interface_block_node)
                    type is (comment_node)
                    type is (directive_node)
                    type is (blank_line_node)
                    type is (implicit_statement_node)
                    type is (end_statement_node)
                class default
                    cycle
                end select
                statement_code = generate_code_from_arena(arena, child_idx)
                if (len(statement_code) == 0) cycle
                if (len(snippet) > 0) snippet = snippet//new_line('A')
                snippet = snippet//statement_code
            end do
        end select
    end function emit_interface_only_program

    logical function program_is_module_wrapper(arena, node) result(is_wrapper)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        integer :: j, child_idx

        is_wrapper = .false.

        if (trim(node%name) /= 'main' .and. trim(node%name) /= '__IMPLICIT_MAIN__') &
            return
        if (.not. allocated(node%body_indices)) return

        do j = 1, size(node%body_indices)
            child_idx = node%body_indices(j)
            if (.not. arena%has_node_at(child_idx)) cycle
            select type (body => arena%entries(child_idx)%node)
                type is (module_node)
                is_wrapper = .true.
                type is (interface_block_node)
                cycle
                type is (function_def_node)
                cycle
                type is (subroutine_def_node)
                cycle
                type is (implicit_statement_node)
                cycle
                type is (contains_node)
                cycle
                type is (end_statement_node)
                cycle
                type is (comment_node)
                cycle
                type is (directive_node)
                cycle
                type is (blank_line_node)
                cycle
            class default
                return
            end select
        end do
    end function program_is_module_wrapper

    subroutine append_module_wrapper(arena, node, code)
        type(ast_arena_t), intent(in) :: arena
        type(program_node), intent(in) :: node
        character(len=:), allocatable, intent(inout) :: code
        integer :: j, child_idx
        character(len=:), allocatable :: module_code

        if (.not. allocated(node%body_indices)) return

        do j = 1, size(node%body_indices)
            child_idx = node%body_indices(j)
            if (.not. arena%has_node_at(child_idx)) cycle
            select type (mod_node => arena%entries(child_idx)%node)
                type is (module_node)
                module_code = generate_code_from_arena(arena, child_idx)
                if (len(module_code) == 0) cycle
                if (len(code) > 0) code = code//new_line('A')// &
                    new_line('A')
                code = code//module_code
            end select
        end do
    end subroutine append_module_wrapper

    recursive function generate_multi_unit_program(arena, node) result(code)
        type(ast_arena_t), intent(in) :: arena
        type(multi_unit_container_node), intent(in) :: node
        character(len=:), allocatable :: code
        integer :: i, child_index
        logical :: has_interface_child
        logical :: has_non_interface_child
        character(len=:), allocatable :: child_code

        code = ""
        if (.not. allocated(node%body_indices)) return
        has_interface_child = .false.
        has_non_interface_child = .false.

        do i = 1, size(node%body_indices)
            child_index = node%body_indices(i)
            if (.not. arena%has_node_at(child_index)) cycle

            select type (child => arena%entries(child_index)%node)
                type is (program_node)
                if (program_is_module_wrapper(arena, child)) then
                    call append_module_wrapper(arena, child, code)
                    cycle
                end if
                if (program_contains_only_interfaces(arena, child_index)) then
                    child_code = emit_interface_only_program(arena, child_index)
                    if (len(child_code) > 0) then
                        if (len(code) > 0) code = code//new_line('A')// &
                            new_line('A')
                        code = code//child_code
                    end if
                    has_interface_child = .true.
                    cycle
                end if
                if (append_trivial_program(arena, code, child_index, &
                    child%name)) cycle
                has_non_interface_child = .true.
                type is (subroutine_def_node)
                if (skip_duplicate_empty_subroutine(arena, node, child, &
                    child_index, i)) cycle
                has_non_interface_child = .true.
                type is (interface_block_node)
                has_interface_child = .true.
            class default
                has_non_interface_child = .true.
            end select

            if (len(code) > 0) code = code//new_line('A')//new_line('A')
            code = code//generate_code_from_arena(arena, child_index)
        end do

        if (has_interface_child .and. .not. has_non_interface_child) then
            if (len_trim(code) > 0) code = code//new_line('A')
            code = code//"end"
        end if
    end function generate_multi_unit_program

    logical function append_trivial_program(arena, code, program_index, name)
        type(ast_arena_t), intent(in) :: arena
        character(len=:), allocatable, intent(inout) :: code
        integer, intent(in) :: program_index
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: snippet

        if (.not. program_is_trivial_wrapper(arena, program_index, name)) then
            append_trivial_program = .false.
            return
        end if

        snippet = collect_trivial_program_trivia(arena, program_index)
        if (len_trim(snippet) > 0) then
            if (len(code) > 0) code = code//new_line('A')//new_line('A')
            code = code//snippet
        end if

        append_trivial_program = .true.
    end function append_trivial_program

    logical function skip_duplicate_empty_subroutine(arena, node, child, &
            child_index, position) &
            result(skip)
        type(ast_arena_t), intent(in) :: arena
        type(multi_unit_container_node), intent(in) :: node
        type(subroutine_def_node), intent(in) :: child
        integer, intent(in) :: child_index
        integer, intent(in) :: position

        if (subroutine_has_body_or_params(child)) then
            skip = .false.
        else
            if (declared_in_prior_interface(arena, node, child_index, &
                position)) then
                skip = .true.
                return
            end if
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
        type(multi_unit_container_node), intent(in) :: node
        character(len=*), intent(in) :: name
        integer, intent(in) :: position
        integer :: j, idx

        found = .false.
        if (.not. allocated(node%body_indices)) return

        do j = 1, position - 1
            idx = node%body_indices(j)
            if (.not. arena%has_node_at(idx)) cycle
            select type (prev => arena%entries(idx)%node)
                type is (subroutine_def_node)
                if (prev%name == name) then
                    found = .true.
                    return
                end if
            end select
        end do
    end function has_prior_subroutine_with_name

    logical function declared_in_prior_interface(arena, node, child_index, &
            position) result(found)
        type(ast_arena_t), intent(in) :: arena
        type(multi_unit_container_node), intent(in) :: node
        integer, intent(in) :: child_index
        integer, intent(in) :: position
        integer :: j, idx

        found = .false.
        if (.not. allocated(node%body_indices)) return

        do j = 1, position - 1
            idx = node%body_indices(j)
            if (.not. arena%has_node_at(idx)) cycle
            select type (prev => arena%entries(idx)%node)
                type is (interface_block_node)
                if (interface_declares_procedure(prev, child_index)) then
                    found = .true.
                    return
                end if
            end select
        end do
    end function declared_in_prior_interface

    logical function interface_declares_procedure(interface_node, proc_index) &
            result(found)
        type(interface_block_node), intent(in) :: interface_node
        integer, intent(in) :: proc_index

        found = .false.
        if (.not. allocated(interface_node%procedure_indices)) return
        if (any(interface_node%procedure_indices == proc_index)) found = .true.
    end function interface_declares_procedure

    logical function program_is_trivial_wrapper(arena, prog_index, name) &
            result(is_trivial)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        character(len=*), intent(in) :: name
        integer :: j, child_idx

        is_trivial = .false.
        if (.not. arena%has_node_at(prog_index)) return

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
                if (.not. arena%has_node_at(child_idx)) cycle
                select type (body => arena%entries(child_idx)%node)
                    type is (comment_node)
                    cycle
                    type is (directive_node)
                    cycle
                    type is (blank_line_node)
                    cycle
                    type is (implicit_statement_node)
                    if (body%is_none) cycle
                    is_trivial = .false.
                    return
                    type is (statement_function_node)
                    cycle
                    type is (end_statement_node)
                    cycle
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
        if (.not. arena%has_node_at(prog_index)) return

        select type (prog => arena%entries(prog_index)%node)
            type is (program_node)
            if (.not. allocated(prog%body_indices)) return
            do j = 1, size(prog%body_indices)
                child_idx = prog%body_indices(j)
                if (.not. arena%has_node_at(child_idx)) cycle
                select type (body => arena%entries(child_idx)%node)
                    type is (comment_node)
                    snippet = generate_code_from_arena(arena, child_idx)
                    type is (directive_node)
                    snippet = generate_code_from_arena(arena, child_idx)
                    type is (blank_line_node)
                    snippet = generate_code_from_arena(arena, child_idx)
                    type is (end_statement_node)
                    snippet = generate_code_from_arena(arena, child_idx)
                    type is (statement_function_node)
                    snippet = generate_code_from_arena(arena, child_idx)
                class default
                    cycle
                end select

                if (len(snippet) > 0) then
                    if (len(trivia_code) > 0) trivia_code = trivia_code// &
                        new_line('A')
                    trivia_code = trivia_code//snippet
                end if
            end do
        end select
    end function collect_trivial_program_trivia

end module codegen_program_generation
