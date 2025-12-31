! @slow-path
module variable_usage_dispatcher_module
    use, intrinsic :: iso_fortran_env, only: error_unit
    use ast_arena_modern
    use variable_usage_core_module

    ! Import control flow handlers
    use variable_usage_control_handlers_module, only: &
        traversal_context_t, push_node, validate_node_index, &
        process_if_node_children, process_do_while_node_children, &
        process_select_case_node_children, process_where_node_children, &
        process_where_stmt_node_children, process_case_block_node_children, &
        process_do_loop_node_children, process_forall_node_children, &
        process_select_rank_node_children, process_rank_block_node_children, &
        process_select_type_node_children, process_type_guard_block_node_children

    ! Import statement handlers
    use variable_usage_stmt_handlers_module, only: &
        process_multi_declaration_node_children, &
        process_print_statement_node_children, &
        process_subroutine_call_children, &
        process_write_statement_children, process_read_statement_children, &
        process_allocate_statement_children, &
        process_deallocate_statement_children, &
        process_associate_construct_children, &
        process_subroutine_def_children, process_function_def_children, &
        process_procedure_def_body

    ! Import expression handlers
    use variable_usage_expr_handlers_module, only: &
        process_binary_op_children, process_call_or_subscript_children, &
        process_array_slice_children, process_component_access_children, &
        process_assignment_node_children, process_program_node_children, &
        process_literal_node_children

    implicit none
    private

    ! Re-export control flow handlers
    public :: process_if_node_children, process_do_while_node_children
    public :: process_select_case_node_children, process_where_node_children
    public :: process_where_stmt_node_children
    public :: process_case_block_node_children
    public :: process_do_loop_node_children, process_forall_node_children
    public :: process_select_rank_node_children, process_rank_block_node_children
    public :: process_select_type_node_children, process_type_guard_block_node_children

    ! Re-export statement handlers
    public :: process_multi_declaration_node_children
    public :: process_print_statement_node_children
    public :: process_subroutine_call_children
    public :: process_write_statement_children, process_read_statement_children
    public :: process_allocate_statement_children
    public :: process_deallocate_statement_children
    public :: process_associate_construct_children
    public :: process_subroutine_def_children, process_function_def_children
    public :: process_procedure_def_body

    ! Re-export expression handlers
    public :: process_binary_op_children, process_call_or_subscript_children
    public :: process_array_slice_children, process_component_access_children
    public :: process_assignment_node_children, process_program_node_children
    public :: process_literal_node_children

    ! Public procedures defined here
    public :: dispatch_node_processing
    public :: collect_identifiers_recursive

contains

    integer function pop_node(ctx) result(node_index)
        type(traversal_context_t), intent(inout) :: ctx

        if (ctx%top <= 0) then
            node_index = 0
            return
        end if

        node_index = ctx%stack(ctx%top)
        ctx%top = ctx%top - 1
    end function pop_node

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

    subroutine dispatch_node_processing(arena, node_index, info, node_type, ctx)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_usage_info_t), intent(inout) :: info
        character(len=*), intent(in) :: node_type
        type(traversal_context_t), intent(inout) :: ctx

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
        case ("select_rank")
            call process_select_rank_node_children(arena, node_index, info, ctx)
        case ("rank_block")
            call process_rank_block_node_children(arena, node_index, info, ctx)
        case ("select_type")
            call process_select_type_node_children(arena, node_index, info, ctx)
        case ("type_guard_block")
            call process_type_guard_block_node_children(arena, node_index, info, ctx)
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

end module variable_usage_dispatcher_module
