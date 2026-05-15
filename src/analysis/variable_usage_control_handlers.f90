! @slow-path
module variable_usage_control_handlers_module
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern
    use variable_usage_core_module
    implicit none
    private

    ! Re-export traversal_context_t from dispatcher core
    type :: traversal_context_t
        integer, allocatable :: stack(:)
        integer :: top = 0
    end type traversal_context_t

    public :: traversal_context_t

    ! Public procedures
    public :: process_if_node_children, process_do_while_node_children
    public :: process_select_case_node_children, process_where_node_children
    public :: process_where_stmt_node_children
    public :: process_case_block_node_children
    public :: process_do_loop_node_children, process_forall_node_children
    public :: process_select_rank_node_children, process_rank_block_node_children
    public :: process_select_type_node_children, process_type_guard_block_node_children
    public :: process_block_construct_node_children

    ! Internal utilities
    public :: push_node, validate_node_index

contains

    logical function validate_node_index(arena, node_index) result(is_valid)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_valid = arena%has_node_at(node_index)
    end function validate_node_index

    subroutine ensure_stack_capacity(ctx)
        type(traversal_context_t), intent(inout) :: ctx
        integer, allocatable :: tmp(:)

        if (.not. allocated(ctx%stack)) then
            allocate (ctx%stack(64))
        else if (ctx%top >= size(ctx%stack)) then
            allocate (tmp(size(ctx%stack) * 2))
            tmp(1:size(ctx%stack)) = ctx%stack
            call move_alloc(tmp, ctx%stack)
        end if
    end subroutine ensure_stack_capacity

    subroutine push_node(ctx, node_index)
        type(traversal_context_t), intent(inout) :: ctx
        integer, intent(in) :: node_index

        if (node_index <= 0) return
        call ensure_stack_capacity(ctx)
        ctx%top = ctx%top + 1
        ctx%stack(ctx%top) = node_index
    end subroutine push_node

    subroutine process_if_node_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: if_node, elseif_wrapper_t
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i, j

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (if_node)
            call push_node(ctx, node%condition_index)

            if (allocated(node%then_body_indices)) then
                do i = 1, size(node%then_body_indices)
                    call push_node(ctx, node%then_body_indices(i))
                end do
            end if

            if (allocated(node%elseif_blocks)) then
                do i = 1, size(node%elseif_blocks)
                    call push_node(ctx, node%elseif_blocks(i)%condition_index)

                    if (allocated(node%elseif_blocks(i)%body_indices)) then
                        do j = 1, size(node%elseif_blocks(i)%body_indices)
                            call push_node(ctx, node%elseif_blocks(i)%body_indices(j))
                        end do
                    end if
                end do
            end if

            if (allocated(node%else_body_indices)) then
                do i = 1, size(node%else_body_indices)
                    call push_node(ctx, node%else_body_indices(i))
                end do
            end if
        end select
    end subroutine process_if_node_children

    subroutine process_do_while_node_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: do_while_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (do_while_node)
            call push_node(ctx, node%condition_index)

            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_do_while_node_children

    subroutine process_select_case_node_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: select_case_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        select type (node => arena%entries(node_index)%node)
        type is (select_case_node)
            call push_node(ctx, node%selector_index)

            if (allocated(node%case_indices)) then
                do i = 1, size(node%case_indices)
                    call push_node(ctx, node%case_indices(i))
                end do
            end if

            call push_node(ctx, node%default_index)
        end select
    end subroutine process_select_case_node_children

    subroutine process_select_rank_node_children(arena, node_index, info, ctx)
        use ast_nodes_conditional, only: select_rank_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        select type (node => arena%entries(node_index)%node)
        type is (select_rank_node)
            call push_node(ctx, node%selector_index)

            if (allocated(node%rank_indices)) then
                do i = 1, size(node%rank_indices)
                    call push_node(ctx, node%rank_indices(i))
                end do
            end if

            call push_node(ctx, node%default_index)
        end select
    end subroutine process_select_rank_node_children

    subroutine process_rank_block_node_children(arena, node_index, info, ctx)
        use ast_nodes_conditional, only: rank_block_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        select type (node => arena%entries(node_index)%node)
        type is (rank_block_node)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_rank_block_node_children

    subroutine process_select_type_node_children(arena, node_index, info, ctx)
        use ast_nodes_conditional, only: select_type_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        select type (node => arena%entries(node_index)%node)
        type is (select_type_node)
            call push_node(ctx, node%selector_index)

            if (allocated(node%guard_indices)) then
                do i = 1, size(node%guard_indices)
                    call push_node(ctx, node%guard_indices(i))
                end do
            end if

            call push_node(ctx, node%default_index)
        end select
    end subroutine process_select_type_node_children

    subroutine process_type_guard_block_node_children(arena, node_index, info, ctx)
        use ast_nodes_conditional, only: type_guard_block_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        select type (node => arena%entries(node_index)%node)
        type is (type_guard_block_node)
            call push_node(ctx, node%type_name_index)

            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_type_guard_block_node_children

    subroutine process_where_node_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: where_node, elsewhere_clause_t
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i, j

        select type (node => arena%entries(node_index)%node)
        type is (where_node)
            call push_node(ctx, node%mask_expr_index)

            if (allocated(node%where_body_indices)) then
                do i = 1, size(node%where_body_indices)
                    call push_node(ctx, node%where_body_indices(i))
                end do
            end if

            if (allocated(node%elsewhere_clauses)) then
                do i = 1, size(node%elsewhere_clauses)
                    call push_node(ctx, node%elsewhere_clauses(i)%mask_index)

                    if (allocated(node%elsewhere_clauses(i)%body_indices)) then
                        do j = 1, size(node%elsewhere_clauses(i)%body_indices)
                            call push_node(ctx, node%elsewhere_clauses(i)% &
                                           body_indices(j))
                        end do
                    end if
                end do
            end if
        end select
    end subroutine process_where_node_children

    subroutine process_where_stmt_node_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: where_stmt_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        select type (node => arena%entries(node_index)%node)
        type is (where_stmt_node)
            call push_node(ctx, node%mask_expr_index)
            call push_node(ctx, node%assignment_index)
        end select
    end subroutine process_where_stmt_node_children

    subroutine process_case_block_node_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: case_block_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (case_block_node)
            if (allocated(node%value_indices)) then
                do i = 1, size(node%value_indices)
                    call push_node(ctx, node%value_indices(i))
                end do
            end if

            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_case_block_node_children

    subroutine process_block_construct_node_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: block_construct_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (block_construct_node)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_block_construct_node_children

    subroutine process_do_loop_node_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: do_loop_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (do_loop_node)
            if (allocated(node%var_name)) then
                call add_string_to_info(node%var_name, node_index, info)
            end if

            call push_node(ctx, node%start_expr_index)
            call push_node(ctx, node%end_expr_index)
            call push_node(ctx, node%step_expr_index)

            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_do_loop_node_children

    subroutine process_forall_node_children(arena, node_index, info, ctx)
        use ast_nodes_loops, only: forall_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (forall_node)
            if (allocated(node%index_names)) then
                do i = 1, node%num_indices
                    call add_string_to_info(node%index_names(i), node_index, info)
                end do
            end if

            if (allocated(node%lower_bound_indices)) then
                do i = 1, node%num_indices
                    call push_node(ctx, node%lower_bound_indices(i))
                end do
            end if

            if (allocated(node%upper_bound_indices)) then
                do i = 1, node%num_indices
                    call push_node(ctx, node%upper_bound_indices(i))
                end do
            end if

            if (allocated(node%stride_indices)) then
                do i = 1, node%num_indices
                    if (node%stride_indices(i) > 0) then
                        call push_node(ctx, node%stride_indices(i))
                    end if
                end do
            end if

            if (node%has_mask .and. node%mask_expr_index > 0) then
                call push_node(ctx, node%mask_expr_index)
            end if

            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_forall_node_children

end module variable_usage_control_handlers_module
