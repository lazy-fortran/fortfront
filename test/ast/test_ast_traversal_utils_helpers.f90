! Helper module for test_ast_traversal_utils callbacks
! Extracted to avoid internal procedure pointer issues
module test_ast_traversal_utils_helpers
    use ast_arena_modern, only: ast_arena_t
    implicit none

    type :: traverse_counter_t
        integer :: count = 0
    end type traverse_counter_t

contains

    subroutine count_callback(arena, node_index, user_data)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(*), intent(inout), optional :: user_data

        if (present(user_data)) then
            select type (user_data)
                type is (traverse_counter_t)
                user_data%count = user_data%count + 1
            end select
        end if
    end subroutine count_callback

end module test_ast_traversal_utils_helpers
