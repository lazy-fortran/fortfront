! @slow-path
module variable_usage_stmt_handlers_module
    use ast_arena_modern
    use variable_usage_core_module
    use variable_usage_control_handlers_module, only: traversal_context_t, &
        push_node, validate_node_index
    implicit none
    private

    ! Public procedures
    public :: process_multi_declaration_node_children
    public :: process_print_statement_node_children
    public :: process_subroutine_call_children
    public :: process_write_statement_children, process_read_statement_children
    public :: process_allocate_statement_children
    public :: process_deallocate_statement_children
    public :: process_associate_construct_children
    public :: process_subroutine_def_children, process_function_def_children
    public :: process_procedure_def_body

contains

    logical function validate_node_with_type(arena, node_index, expected_type) &
            result(is_valid)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: expected_type

        is_valid = .false.

        if (.not. validate_node_index(arena, node_index)) return
        if (node_index > size(arena%entries)) return

        if (arena%entries(node_index)%node_type /= expected_type) return

        is_valid = .true.
    end function validate_node_with_type

    subroutine process_multi_declaration_node_children(arena, node_index, info, ctx)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (declaration_node)
            if (node%is_multi_declaration) then
                if (node%has_initializer) then
                    call push_node(ctx, node%initializer_index)
                end if
            end if
        end select
    end subroutine process_multi_declaration_node_children

    subroutine process_print_statement_node_children(arena, node_index, info, ctx)
        use ast_nodes_io, only: print_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (print_statement_node)
            if (allocated(node%expression_indices)) then
                do i = 1, size(node%expression_indices)
                    call push_node(ctx, node%expression_indices(i))
                end do
            end if
        end select
    end subroutine process_print_statement_node_children

    subroutine process_subroutine_call_children(arena, node_index, info, ctx)
        use ast_nodes_procedure, only: subroutine_call_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        if (arena%entries(node_index)%node_type /= "subroutine_call" .and. &
            arena%entries(node_index)%node_type /= "call_statement") then
            return
        end if

        select type (node => arena%entries(node_index)%node)
            type is (subroutine_call_node)
            if (allocated(node%name)) then
                call add_string_to_info(node%name, node_index, info)
            end if

            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    call push_node(ctx, node%arg_indices(i))
                end do
            end if
        end select
    end subroutine process_subroutine_call_children

    subroutine process_write_statement_children(arena, node_index, info, ctx)
        use ast_nodes_io, only: write_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        if (arena%entries(node_index)%node_type /= "write_statement") then
            return
        end if

        select type (node => arena%entries(node_index)%node)
            type is (write_statement_node)
            call push_node(ctx, node%format_expr_index)
            call push_node(ctx, node%iostat_var_index)

            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    call push_node(ctx, node%arg_indices(i))
                end do
            end if
        end select
    end subroutine process_write_statement_children

    subroutine process_read_statement_children(arena, node_index, info, ctx)
        use ast_nodes_io, only: read_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        if (arena%entries(node_index)%node_type /= "read_statement") then
            return
        end if

        select type (node => arena%entries(node_index)%node)
            type is (read_statement_node)
            call push_node(ctx, node%format_expr_index)
            call push_node(ctx, node%iostat_var_index)

            if (allocated(node%var_indices)) then
                do i = 1, size(node%var_indices)
                    call push_node(ctx, node%var_indices(i))
                end do
            end if
        end select
    end subroutine process_read_statement_children

    subroutine process_allocate_statement_children(arena, node_index, info, ctx)
        use ast_nodes_misc, only: allocate_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_with_type(arena, node_index, "allocate_statement")) &
            return

        select type (node => arena%entries(node_index)%node)
            type is (allocate_statement_node)
            if (allocated(node%var_indices)) then
                do i = 1, size(node%var_indices)
                    call push_node(ctx, node%var_indices(i))
                end do
            end if

            if (allocated(node%shape_indices)) then
                do i = 1, size(node%shape_indices)
                    call push_node(ctx, node%shape_indices(i))
                end do
            end if

            call push_node(ctx, node%stat_var_index)
            call push_node(ctx, node%errmsg_var_index)
            call push_node(ctx, node%source_expr_index)
            call push_node(ctx, node%mold_expr_index)
        end select
    end subroutine process_allocate_statement_children

    subroutine process_deallocate_statement_children(arena, node_index, info, ctx)
        use ast_nodes_misc, only: deallocate_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_with_type(arena, node_index, "deallocate_statement")) &
            return

        select type (node => arena%entries(node_index)%node)
            type is (deallocate_statement_node)
            if (allocated(node%var_indices)) then
                do i = 1, size(node%var_indices)
                    call push_node(ctx, node%var_indices(i))
                end do
            end if

            call push_node(ctx, node%stat_var_index)
            call push_node(ctx, node%errmsg_var_index)
        end select
    end subroutine process_deallocate_statement_children

    subroutine process_associate_construct_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: associate_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (associate_node)
            if (allocated(node%associations)) then
                do i = 1, size(node%associations)
                    call push_node(ctx, node%associations(i)%expr_index)
                end do
            end if

            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_associate_construct_children

    subroutine process_subroutine_def_children(arena, node_index, info, ctx)
        use ast_nodes_procedure, only: subroutine_def_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (subroutine_def_node)
            if (allocated(node%body_indices)) then
                call process_procedure_def_body(arena, node_index, info, &
                    node%body_indices, ctx)
            end if
        end select
    end subroutine process_subroutine_def_children

    subroutine process_function_def_children(arena, node_index, info, ctx)
        use ast_nodes_procedure, only: function_def_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (function_def_node)
            if (allocated(node%body_indices)) then
                call process_procedure_def_body(arena, node_index, info, &
                    node%body_indices, ctx)
            end if
        end select
    end subroutine process_function_def_children

    subroutine process_procedure_def_body(arena, node_index, info, body_indices, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        integer, intent(in), optional :: body_indices(:)
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (present(body_indices)) then
            do i = 1, size(body_indices)
                call push_node(ctx, body_indices(i))
            end do
        end if
    end subroutine process_procedure_def_body

end module variable_usage_stmt_handlers_module
