module codegen_program_generation
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: program_node
    use ast_nodes_misc, only: blank_line_node, comment_node, contains_node, &
                              directive_node, &
                              implicit_statement_node
    use ast_nodes_procedure, only: subroutine_def_node
    use codegen_arena_interface, only: generate_code_from_arena
    use codegen_program_body, only: append_program_body
    use codegen_program_header, only: assemble_program_header
    implicit none
    private
    public :: generate_code_program

contains

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
            type is (directive_node)
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
                type is (directive_node)
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
                type is (directive_node)
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

end module codegen_program_generation
