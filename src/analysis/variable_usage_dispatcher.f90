! @slow-path
module variable_usage_dispatcher_module
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern
    use variable_usage_core_module
    use ast_nodes_core, only: binary_op_node, call_or_subscript_node, &
                              identifier_node, component_access_node, &
                              assignment_node, program_node, literal_node
    use ast_nodes_bounds, only: array_slice_node
    implicit none
    private

    type :: traversal_context_t
        integer, allocatable :: stack(:)
        integer :: top = 0
    end type traversal_context_t

    ! Public procedures
    public :: dispatch_node_processing
    public :: process_if_node_children, process_do_while_node_children
    public :: process_select_case_node_children, process_where_node_children
    public :: process_where_stmt_node_children, process_multi_declaration_node_children
    public :: process_print_statement_node_children, process_case_block_node_children
    public :: process_do_loop_node_children, process_forall_node_children
    public :: process_subroutine_call_children
    public :: process_write_statement_children, process_read_statement_children
    public :: process_allocate_statement_children, process_deallocate_statement_children
    public :: process_associate_construct_children, process_subroutine_def_children
    public :: process_function_def_children
    public :: process_binary_op_children, process_call_or_subscript_children
    public :: process_array_slice_children, process_component_access_children
    public :: process_assignment_node_children, process_program_node_children
    public :: process_literal_node_children, process_procedure_def_body

    ! Public interface for recursive collection
    public :: collect_identifiers_recursive

contains

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

    logical function validate_node_index(arena, node_index) result(is_valid)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        is_valid = .false.
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries)) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        is_valid = .true.
    end function validate_node_index

    logical function validate_node_with_type(arena, node_index, expected_type) &
        result(is_valid)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: expected_type

        is_valid = .false.

        if (.not. validate_node_index(arena, node_index)) return
        if (node_index > size(arena%entries)) return

        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= expected_type) return

        is_valid = .true.
    end function validate_node_with_type

    subroutine push_node(ctx, node_index)
        type(traversal_context_t), intent(inout) :: ctx
        integer, intent(in) :: node_index

        if (node_index <= 0) return
        call ensure_stack_capacity(ctx)
        ctx%top = ctx%top + 1
        ctx%stack(ctx%top) = node_index
    end subroutine push_node

    integer function pop_node(ctx) result(node_index)
        type(traversal_context_t), intent(inout) :: ctx

        if (ctx%top <= 0) then
            node_index = 0
            return
        end if

        node_index = ctx%stack(ctx%top)
        ctx%top = ctx%top - 1
    end function pop_node

    ! Iteratively collect all identifier nodes from expression subtree
    subroutine collect_identifiers_recursive(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info

        type(traversal_context_t) :: ctx
        logical, allocatable :: visited(:)
        character(len=:), allocatable :: node_type
        integer :: current

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        if (arena%size > 0) then
            allocate (visited(arena%size))
            visited = .false.
        end if

        call push_node(ctx, node_index)

        do while (ctx%top > 0)
            current = pop_node(ctx)
            if (current <= 0 .or. current > arena%size) cycle
            if (.not. allocated(arena%entries(current)%node)) cycle
            if (allocated(visited)) then
                if (visited(current)) cycle
                visited(current) = .true.
            end if

            node_type = arena%entries(current)%node_type

            if (node_type == "identifier") then
                call add_identifier_to_info(arena, current, info)
            end if

            call dispatch_node_processing(arena, current, info, node_type, ctx)
        end do

        if (allocated(visited)) then
            deallocate (visited)
        end if
    end subroutine collect_identifiers_recursive

    ! Dispatch node processing based on node type
    subroutine dispatch_node_processing(arena, node_index, info, node_type, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        character(len=*), intent(in) :: node_type
        type(traversal_context_t), intent(inout) :: ctx

        ! Traverse all child nodes based on node type
        select case (node_type)
        case ("binary_op")
            call process_binary_op_children(arena, node_index, info, ctx)
        case ("call_or_subscript")
            call process_call_or_subscript_children(arena, node_index, info, ctx)
        case ("array_slice")
            call process_array_slice_children(arena, node_index, info, ctx)
        case ("component_access")
            call process_component_access_children(arena, node_index, info, ctx)
        case ("if", "if_statement")
            call process_if_node_children(arena, node_index, info, ctx)
        case ("do_while")
            call process_do_while_node_children(arena, node_index, info, ctx)
        case ("select_case")
            call process_select_case_node_children(arena, node_index, info, ctx)
        case ("where")
            call process_where_node_children(arena, node_index, info, ctx)
        case ("where_stmt")
            call process_where_stmt_node_children(arena, node_index, info, ctx)
        case ("program")
            call process_program_node_children(arena, node_index, info, ctx)
        case ("literal")
            call process_literal_node_children(arena, node_index, info, ctx)
        case ("multi_declaration")
            call process_multi_declaration_node_children(arena, node_index, info, ctx)
        case ("print_statement")
            call process_print_statement_node_children(arena, node_index, info, ctx)
        case ("case_block")
            call process_case_block_node_children(arena, node_index, info, ctx)
        case ("do_loop")
            call process_do_loop_node_children(arena, node_index, info, ctx)
        case ("forall")
            call process_forall_node_children(arena, node_index, info, ctx)
        case ("assignment")
            call process_assignment_node_children(arena, node_index, info, ctx)
        case ("subroutine_call", "call_statement")
            call process_subroutine_call_children(arena, node_index, info, ctx)
        case ("write_statement")
            call process_write_statement_children(arena, node_index, info, ctx)
        case ("read_statement")
            call process_read_statement_children(arena, node_index, info, ctx)
        case ("allocate_statement")
            call process_allocate_statement_children(arena, node_index, info, ctx)
        case ("deallocate_statement")
            call process_deallocate_statement_children(arena, node_index, info, ctx)
        case ("associate")
            call process_associate_construct_children(arena, node_index, info, ctx)
        case ("subroutine_def")
            call process_subroutine_def_children(arena, node_index, info, ctx)
        case ("function_def")
            call process_function_def_children(arena, node_index, info, ctx)
        end select
    end subroutine dispatch_node_processing

    ! Process if node children
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
            ! Process condition expression
            call push_node(ctx, node%condition_index)

            ! Process then body statements
            if (allocated(node%then_body_indices)) then
                do i = 1, size(node%then_body_indices)
                    call push_node(ctx, node%then_body_indices(i))
                end do
            end if

            ! Process elseif blocks
            if (allocated(node%elseif_blocks)) then
                do i = 1, size(node%elseif_blocks)
                    ! Process elseif condition
                    call push_node(ctx, node%elseif_blocks(i)%condition_index)

                    ! Process elseif body
                    if (allocated(node%elseif_blocks(i)%body_indices)) then
                        do j = 1, size(node%elseif_blocks(i)%body_indices)
                            call push_node(ctx, node%elseif_blocks(i)%body_indices(j))
                        end do
                    end if
                end do
            end if

            ! Process else body statements
            if (allocated(node%else_body_indices)) then
                do i = 1, size(node%else_body_indices)
                    call push_node(ctx, node%else_body_indices(i))
                end do
            end if
        end select
    end subroutine process_if_node_children

    ! Process do while node children
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
            ! Process condition expression
            call push_node(ctx, node%condition_index)

            ! Process body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_do_while_node_children

    ! Process select case node children
    subroutine process_select_case_node_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: select_case_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        select type (node => arena%entries(node_index)%node)
        type is (select_case_node)
            ! Process selector expression
            call push_node(ctx, node%selector_index)

            ! Process case blocks
            if (allocated(node%case_indices)) then
                do i = 1, size(node%case_indices)
                    call push_node(ctx, node%case_indices(i))
                end do
            end if

            ! Process default case
            call push_node(ctx, node%default_index)
        end select
    end subroutine process_select_case_node_children

    ! Process where node children
    subroutine process_where_node_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: where_node, elsewhere_clause_t
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i, j

        select type (node => arena%entries(node_index)%node)
        type is (where_node)
            ! Process mask expression
            call push_node(ctx, node%mask_expr_index)

            ! Process where body statements
            if (allocated(node%where_body_indices)) then
                do i = 1, size(node%where_body_indices)
                    call push_node(ctx, node%where_body_indices(i))
                end do
            end if

            ! Process elsewhere clauses
            if (allocated(node%elsewhere_clauses)) then
                do i = 1, size(node%elsewhere_clauses)
                    ! Process elsewhere mask if present
                    call push_node(ctx, node%elsewhere_clauses(i)%mask_index)

                    ! Process elsewhere body
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

    ! Process where statement node children
    subroutine process_where_stmt_node_children(arena, node_index, info, ctx)
        use ast_nodes_control, only: where_stmt_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        select type (node => arena%entries(node_index)%node)
        type is (where_stmt_node)
            ! Process mask expression
            call push_node(ctx, node%mask_expr_index)

            ! Process assignment
            call push_node(ctx, node%assignment_index)
        end select
    end subroutine process_where_stmt_node_children

    ! Process multi declaration node children
    subroutine process_multi_declaration_node_children(arena, node_index, info, ctx)
        use ast_nodes_data, only: declaration_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (declaration_node)
            ! Only process if this is actually a multi-declaration
            if (node%is_multi_declaration) then
                ! Process initialization expression if present
                if (node%has_initializer) then
                    call push_node(ctx, node%initializer_index)
                end if
            end if
        end select
    end subroutine process_multi_declaration_node_children

    ! Process print statement node children
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
            ! Process all expression arguments
            if (allocated(node%expression_indices)) then
                do i = 1, size(node%expression_indices)
                    call push_node(ctx, node%expression_indices(i))
                end do
            end if
        end select
    end subroutine process_print_statement_node_children

    ! Process case block node children
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
            ! Process case value expressions
            if (allocated(node%value_indices)) then
                do i = 1, size(node%value_indices)
                    call push_node(ctx, node%value_indices(i))
                end do
            end if

            ! Process case body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_case_block_node_children

    ! Process do loop node children
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
            ! Process loop variable (stored as string, not index)
            if (allocated(node%var_name)) then
                call add_string_to_info(node%var_name, node_index, info)
            end if

            ! Process start expression
            call push_node(ctx, node%start_expr_index)

            ! Process end expression
            call push_node(ctx, node%end_expr_index)

            ! Process step expression
            call push_node(ctx, node%step_expr_index)

            ! Process loop body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_do_loop_node_children

    ! Process forall node children
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
            ! Process forall index variables (stored as strings, not indices)
            if (allocated(node%index_names)) then
                do i = 1, node%num_indices
                    call add_string_to_info(node%index_names(i), node_index, info)
                end do
            end if

            ! Process lower bound expressions
            if (allocated(node%lower_bound_indices)) then
                do i = 1, node%num_indices
                    call push_node(ctx, node%lower_bound_indices(i))
                end do
            end if

            ! Process upper bound expressions
            if (allocated(node%upper_bound_indices)) then
                do i = 1, node%num_indices
                    call push_node(ctx, node%upper_bound_indices(i))
                end do
            end if

            ! Process stride expressions
            if (allocated(node%stride_indices)) then
                do i = 1, node%num_indices
                    if (node%stride_indices(i) > 0) then
                        call push_node(ctx, node%stride_indices(i))
                    end if
                end do
            end if

            ! Process mask expression if present
            if (node%has_mask .and. node%mask_expr_index > 0) then
                call push_node(ctx, node%mask_expr_index)
            end if

            ! Process forall body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_forall_node_children

    ! Process subroutine call children
    subroutine process_subroutine_call_children(arena, node_index, info, ctx)
        use ast_nodes_procedure, only: subroutine_call_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= "subroutine_call" .and. &
            arena%entries(node_index)%node_type /= "call_statement") then
            ! Node type mismatch - this shouldn't happen if called correctly
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (subroutine_call_node)
            ! Subroutine name is stored as string, not index
            if (allocated(node%name)) then
                call add_string_to_info(node%name, node_index, info)
            end if

            ! Process all arguments
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    call push_node(ctx, node%arg_indices(i))
                end do
            end if
        end select
    end subroutine process_subroutine_call_children

    ! Process write statement children
    subroutine process_write_statement_children(arena, node_index, info, ctx)
        use ast_nodes_io, only: write_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= "write_statement") then
            ! Node type mismatch - this shouldn't happen if called correctly
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (write_statement_node)
            ! Process runtime format expression if present
            call push_node(ctx, node%format_expr_index)

            ! Process iostat variable if present
            call push_node(ctx, node%iostat_var_index)

            ! Process all output arguments
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    call push_node(ctx, node%arg_indices(i))
                end do
            end if
        end select
    end subroutine process_write_statement_children

    ! Process read statement children
    subroutine process_read_statement_children(arena, node_index, info, ctx)
        use ast_nodes_io, only: read_statement_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= "read_statement") then
            ! Node type mismatch - this shouldn't happen if called correctly
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (read_statement_node)
            ! Process runtime format expression if present
            call push_node(ctx, node%format_expr_index)

            ! Process iostat variable if present
            call push_node(ctx, node%iostat_var_index)

            ! Process all variables to read into
            if (allocated(node%var_indices)) then
                do i = 1, size(node%var_indices)
                    call push_node(ctx, node%var_indices(i))
                end do
            end if
        end select
    end subroutine process_read_statement_children

    ! Process allocate statement children
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
            ! Process variables being allocated
            if (allocated(node%var_indices)) then
                do i = 1, size(node%var_indices)
                    call push_node(ctx, node%var_indices(i))
                end do
            end if

            ! Process shape expressions for each variable
            if (allocated(node%shape_indices)) then
                do i = 1, size(node%shape_indices)
                    call push_node(ctx, node%shape_indices(i))
                end do
            end if

            ! Process stat variable if present
            call push_node(ctx, node%stat_var_index)

            ! Process errmsg variable if present
            call push_node(ctx, node%errmsg_var_index)

            ! Process source expression if present
            call push_node(ctx, node%source_expr_index)

            ! Process mold expression if present
            call push_node(ctx, node%mold_expr_index)
        end select
    end subroutine process_allocate_statement_children

    ! Process deallocate statement children
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
            ! Process variables being deallocated
            if (allocated(node%var_indices)) then
                do i = 1, size(node%var_indices)
                    call push_node(ctx, node%var_indices(i))
                end do
            end if

            ! Process stat variable if present
            call push_node(ctx, node%stat_var_index)

            ! Process errmsg variable if present
            call push_node(ctx, node%errmsg_var_index)
        end select
    end subroutine process_deallocate_statement_children

    ! Process associate construct children
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
            ! Process all associations - track variables used in target expressions
            if (allocated(node%associations)) then
                do i = 1, size(node%associations)
                    ! Process the target expression - this tracks the original variable
                    ! The alias name itself is NOT a variable usage, it's a new binding
                    call push_node(ctx, node%associations(i)%expr_index)
                end do
            end if

            ! Process the body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_associate_construct_children

    ! Process subroutine definition children
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

    ! Process function definition children
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

    ! Process binary operation children
    subroutine process_binary_op_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        select type (node => arena%entries(node_index)%node)
        type is (binary_op_node)
            call push_node(ctx, node%left_index)
            call push_node(ctx, node%right_index)
        end select
    end subroutine process_binary_op_children

    ! Process call or subscript children
    subroutine process_call_or_subscript_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        select type (node => arena%entries(node_index)%node)
        type is (call_or_subscript_node)
            ! The function/array name is stored as a string, not an index
            ! We need to add it manually to the info if it's a variable reference
            if (allocated(node%name)) then
                call add_string_to_info(node%name, node_index, info)
            end if

            ! Process all arguments/subscripts
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    if (node%arg_indices(i) <= 0) cycle
                    if (.not. allocated(arena%entries(node%arg_indices(i))%node)) cycle
                    select type (arg => arena%entries(node%arg_indices(i))%node)
                    type is (assignment_node)
                        call push_node(ctx, arg%value_index)
                    class default
                        call push_node(ctx, node%arg_indices(i))
                    end select
                end do
            end if
        end select
    end subroutine process_call_or_subscript_children

    ! Process array slice children
    subroutine process_array_slice_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        select type (node => arena%entries(node_index)%node)
        type is (array_slice_node)
            ! Process array name
            call push_node(ctx, node%array_index)

            ! Process slice bounds
            block
                integer :: i
                do i = 1, node%num_dimensions
                    call push_node(ctx, node%bounds_indices(i))
                end do
            end block
        end select
    end subroutine process_array_slice_children

    ! Process component access children
    subroutine process_component_access_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        select type (node => arena%entries(node_index)%node)
        type is (component_access_node)
            ! Process the base expression (structure/derived type)
            call push_node(ctx, node%base_expr_index)

            ! Component name is stored as a string
            if (allocated(node%component_name)) then
                call add_string_to_info(node%component_name, node_index, info)
            end if
        end select
    end subroutine process_component_access_children

    ! Process assignment node children
    subroutine process_assignment_node_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        if (.not. validate_node_index(arena, node_index)) return

        ! Verify node type matches expectation
        if (arena%entries(node_index)%node_type /= "assignment") then
            ! Node type mismatch - this shouldn't happen if called correctly
            return
        end if

        select type (node => arena%entries(node_index)%node)
        type is (assignment_node)
            ! Keyword arguments should not introduce new targets for declaration
            if (node%is_keyword_argument) then
                call push_node(ctx, node%value_index)
                return
            end if

            ! Process target (LHS) - might have array subscripts
            call push_node(ctx, node%target_index)

            ! Process value (RHS)
            call push_node(ctx, node%value_index)
        end select
    end subroutine process_assignment_node_children

    ! Process program node children
    subroutine process_program_node_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        if (.not. validate_node_index(arena, node_index)) return

        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            ! Process all body statements
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call push_node(ctx, node%body_indices(i))
                end do
            end if
        end select
    end subroutine process_program_node_children

    ! Process literal node children (might contain parsed expressions)
    subroutine process_literal_node_children(arena, node_index, info, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        type(traversal_context_t), intent(inout) :: ctx

        ! Literals typically don't have child nodes with expressions
        ! But we should check if this literal represents a parsed statement
        ! that might contain identifiers (like an if statement)

        ! For now, we don't traverse literal nodes as they usually contain
        ! constant values, not variable references
    end subroutine process_literal_node_children

    ! Process procedure definition children (shared by subroutine and function)
    subroutine process_procedure_def_body(arena, node_index, info, body_indices, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        integer, intent(in), optional :: body_indices(:)
        type(traversal_context_t), intent(inout) :: ctx

        integer :: i

        ! Process all body statements - this will capture all identifiers used
        ! within the procedure, including dummy arguments when they're used
        if (present(body_indices)) then
            do i = 1, size(body_indices)
                call push_node(ctx, body_indices(i))
            end do
        end if
    end subroutine process_procedure_def_body

end module variable_usage_dispatcher_module
