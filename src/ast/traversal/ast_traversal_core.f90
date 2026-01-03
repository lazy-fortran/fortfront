module ast_traversal_core
    use ast_arena_modern, only: ast_arena_t
    use ast_base, only: ast_node
    use ast_traversal_gather, only: gather_child_indices
    use ast_traversal_visit, only: visit_node
    use ast_visitor, only: ast_visitor_t
    implicit none
    private

    public :: traverse_ast, traverse_preorder, traverse_postorder

contains

    ! Main traversal entry point
    subroutine traverse_ast(arena, root_index, visitor)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        class(ast_visitor_t), intent(inout) :: visitor

        ! Default to pre-order traversal
        call traverse_preorder(arena, root_index, visitor)
    end subroutine traverse_ast

    ! Pre-order traversal (visit node before children)
    subroutine traverse_preorder(arena, node_index, visitor)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(ast_visitor_t), intent(inout) :: visitor

        integer, allocatable :: stack(:)
        integer :: top, capacity
        integer :: current_index
        integer, allocatable :: children(:)
        integer :: i

        if (.not. arena%has_node_at(node_index)) return

        capacity = 128
        allocate (stack(capacity))
        top = 0
        call push(node_index)

        do while (top > 0)
            current_index = stack(top)
            top = top - 1

            if (.not. arena%has_node_at(current_index)) cycle

            select type (node => arena%entries(current_index)%node)
            class is (ast_node)
                call visit_node(node, visitor)
            end select

            call gather_child_indices(arena, current_index, children)
            if (size(children) > 0) then
                do i = size(children), 1, -1
                    call push(children(i))
                end do
            end if
            if (allocated(children)) deallocate (children)
        end do

    contains

        subroutine push(idx)
            integer, intent(in) :: idx
            integer, allocatable :: tmp(:)

            if (idx <= 0) return
            if (top >= capacity) then
                capacity = capacity * 2
                allocate (tmp(capacity))
                if (top > 0) tmp(1:top) = stack(1:top)
                call move_alloc(tmp, stack)
            end if
            top = top + 1
            stack(top) = idx
        end subroutine push

    end subroutine traverse_preorder

    ! Post-order traversal (visit children before node)
    subroutine traverse_postorder(arena, node_index, visitor)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(ast_visitor_t), intent(inout) :: visitor

        integer, allocatable :: stack(:)
        integer, allocatable :: output(:)
        integer :: top_stack, top_output, capacity_stack, capacity_output
        integer :: current_index
        integer, allocatable :: children(:)
        integer :: i

        if (.not. arena%has_node_at(node_index)) return

        capacity_stack = 128
        capacity_output = 128
        allocate (stack(capacity_stack))
        allocate (output(capacity_output))
        top_stack = 0
        top_output = 0
        call push_stack(node_index)

        do while (top_stack > 0)
            current_index = stack(top_stack)
            top_stack = top_stack - 1

            if (.not. arena%has_node_at(current_index)) cycle

            call push_output(current_index)

            call gather_child_indices(arena, current_index, children)
            if (size(children) > 0) then
                do i = 1, size(children)
                    call push_stack(children(i))
                end do
            end if
            if (allocated(children)) deallocate (children)
        end do

        do while (top_output > 0)
            current_index = output(top_output)
            top_output = top_output - 1

            select type (node => arena%entries(current_index)%node)
            class is (ast_node)
                call visit_node(node, visitor)
            end select
        end do

    contains

        subroutine push_stack(idx)
            integer, intent(in) :: idx
            integer, allocatable :: tmp(:)

            if (idx <= 0) return
            if (top_stack >= capacity_stack) then
                capacity_stack = capacity_stack * 2
                allocate (tmp(capacity_stack))
                if (top_stack > 0) tmp(1:top_stack) = stack(1:top_stack)
                call move_alloc(tmp, stack)
            end if
            top_stack = top_stack + 1
            stack(top_stack) = idx
        end subroutine push_stack

        subroutine push_output(idx)
            integer, intent(in) :: idx
            integer, allocatable :: tmp(:)

            if (idx <= 0) return
            if (top_output >= capacity_output) then
                capacity_output = capacity_output * 2
                allocate (tmp(capacity_output))
                if (top_output > 0) tmp(1:top_output) = output(1:top_output)
                call move_alloc(tmp, output)
            end if
            top_output = top_output + 1
            output(top_output) = idx
        end subroutine push_output

    end subroutine traverse_postorder

end module ast_traversal_core

