module frontend_compiler_queries
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_procedure, only: subroutine_call_node
    implicit none
    private

    public :: is_subroutine_call_statement
    public :: get_subroutine_call_name
    public :: get_subroutine_call_arg_indices

contains

    logical function is_subroutine_call_statement(arena, node_index) result(is_call)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_call = .false.
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (subroutine_call_node)
            is_call = .true.
        end select
    end function is_subroutine_call_statement

    subroutine get_subroutine_call_name(arena, node_index, name, error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=:), allocatable, intent(out) :: name
        character(len=:), allocatable, intent(out) :: error_msg

        call set_empty(name)
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'subroutine call index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (subroutine_call_node)
            if (.not. allocated(node%name)) then
                error_msg = 'subroutine call node has no callee name'
                return
            end if
            name = node%name
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not an explicit subroutine CALL statement'
        end select
    end subroutine get_subroutine_call_name

    subroutine get_subroutine_call_arg_indices(arena, node_index, arg_indices, &
                                               error_msg)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(out) :: arg_indices(:)
        character(len=:), allocatable, intent(out) :: error_msg

        allocate (arg_indices(0))
        if (.not. arena%has_node_at(node_index)) then
            error_msg = 'subroutine call index does not reference an AST node'
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (subroutine_call_node)
            if (allocated(node%arg_indices)) then
                if (size(node%arg_indices) > 0) arg_indices = node%arg_indices
            end if
            call set_empty(error_msg)
        class default
            error_msg = 'AST node is not an explicit subroutine CALL statement'
        end select
    end subroutine get_subroutine_call_arg_indices

    subroutine set_empty(value)
        character(len=:), allocatable, intent(out) :: value

        allocate (character(len=0) :: value)
    end subroutine set_empty

end module frontend_compiler_queries
