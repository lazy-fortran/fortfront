module standardizer_wrapping
    use ast_arena_modern, only: ast_arena_t
    use ast_factory, only: push_implicit_statement
    use ast_nodes_core, only: program_node
    use ast_nodes_misc, only: contains_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use standardizer_function, only: standardize_function_def
    use standardizer_parameter, only: node_exists
    use standardizer_subroutine, only: standardize_subroutine_def
    implicit none
    private
    public :: wrap_function_in_program
    public :: wrap_subroutine_in_program
contains

    subroutine wrap_function_in_program(arena, func_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: func_index

        call wrap_subprogram_common(arena, func_index, .true.)
    end subroutine wrap_function_in_program

    subroutine wrap_subroutine_in_program(arena, sub_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: sub_index

        call wrap_subprogram_common(arena, sub_index, .false.)
    end subroutine wrap_subroutine_in_program

    subroutine wrap_subprogram_common(arena, member_index, is_function)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(inout) :: member_index
        logical, intent(in) :: is_function
        type(program_node) :: prog
        type(contains_node) :: contains_stmt
        integer :: implicit_none_index
        integer :: contains_index
        integer :: prog_index

        implicit_none_index = push_implicit_statement(arena, .true., &
            line=1, column=1, &
            parent_index=0)
        contains_stmt%line = 1
        contains_stmt%column = 1
        call arena%push(contains_stmt, "contains", 0)
        contains_index = arena%size

        call standardize_wrapper_member(arena, member_index, is_function)
        call initialize_wrapper_program(prog, implicit_none_index, &
            contains_index, &
            member_index)

        call arena%push(prog, "program", 0)
        prog_index = arena%size
        call update_wrapper_parents(arena, implicit_none_index, contains_index, &
            member_index, prog_index)
        member_index = prog_index
    end subroutine wrap_subprogram_common

    subroutine standardize_wrapper_member(arena, member_index, is_function)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: member_index
        logical, intent(in) :: is_function

        if (.not. node_exists(arena, member_index)) return

        if (is_function) then
            select type (member => arena%entries(member_index)%node)
                type is (function_def_node)
                call standardize_function_def(arena, member, member_index)
            end select
        else
            select type (member => arena%entries(member_index)%node)
                type is (subroutine_def_node)
                call standardize_subroutine_def(arena, member, member_index)
            end select
        end if
    end subroutine standardize_wrapper_member

    subroutine initialize_wrapper_program(prog, implicit_none_index, &
            contains_index, &
            member_index)
        type(program_node), intent(inout) :: prog
        integer, intent(in) :: implicit_none_index
        integer, intent(in) :: contains_index
        integer, intent(in) :: member_index
        integer, allocatable :: body_indices(:)

        prog%name = "main"
        prog%line = 1
        prog%column = 1
        allocate (body_indices(3))
        body_indices(1) = implicit_none_index
        body_indices(2) = contains_index
        body_indices(3) = member_index
        prog%body_indices = body_indices
    end subroutine initialize_wrapper_program

    subroutine update_wrapper_parents(arena, implicit_index, contains_index, &
            member_index, program_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: implicit_index
        integer, intent(in) :: contains_index
        integer, intent(in) :: member_index
        integer, intent(in) :: program_index

        if (node_exists(arena, implicit_index)) then
            arena%entries(implicit_index)%parent_index = program_index
        end if
        if (node_exists(arena, contains_index)) then
            arena%entries(contains_index)%parent_index = program_index
        end if
        if (node_exists(arena, member_index)) then
            arena%entries(member_index)%parent_index = program_index
        end if
    end subroutine update_wrapper_parents

end module standardizer_wrapping
