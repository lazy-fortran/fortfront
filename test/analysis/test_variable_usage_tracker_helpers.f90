! Helper module for test_variable_usage_tracker callbacks
! Extracted to avoid internal procedure pointer issues
module test_variable_usage_tracker_helpers
    use ast_arena_modern, only: ast_arena_t
    implicit none

contains

    ! Visitor function to count nodes
    subroutine count_nodes_visitor(arena, node_index, node_type, user_data)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: node_type
        class(*), intent(inout), optional :: user_data

        if (present(user_data)) then
            select type (counter => user_data)
                type is (integer)
                counter = counter + 1
            end select
        end if
    end subroutine count_nodes_visitor

end module test_variable_usage_tracker_helpers
