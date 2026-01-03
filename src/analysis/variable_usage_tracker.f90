! @slow-path
module variable_usage_tracker_module
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_arena_modern
    use variable_usage_core_module
    use variable_usage_dispatcher_module
    use ast_nodes_core, only: binary_op_node, call_or_subscript_node, &
                              identifier_node, component_access_node
    implicit none
    private

    ! Re-export types from core module
    public :: variable_usage_info_t, expression_visitor_t

    ! Public procedures
    public :: create_variable_usage_info, get_variables_in_expression
    public :: get_identifiers_in_subtree, visit_expression_nodes
    public :: is_variable_used_in_expression, count_variable_usage

contains

    ! Get all variables used in an expression
    function get_variables_in_expression(arena, expr_index) result(info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        type(variable_usage_info_t) :: info

        info = create_variable_usage_info()

        if (.not. arena%has_node_at(expr_index)) return

        call collect_identifiers_recursive(arena, expr_index, info)
    end function get_variables_in_expression

    ! Get list of all identifiers in a subtree (convenience function)
    function get_identifiers_in_subtree(arena, root_index) result(identifiers)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        character(len=:), allocatable :: identifiers(:)

        type(variable_usage_info_t) :: info

        info = get_variables_in_expression(arena, root_index)

        if (allocated(info%variable_names) .and. size(info%variable_names) > 0) then
            identifiers = info%variable_names
        else
            allocate (character(len=0) :: identifiers(0))
        end if
    end function get_identifiers_in_subtree

    ! Visit all expression nodes with a visitor function
    subroutine visit_expression_nodes(arena, root_index, visitor, user_data)
        use ast_nodes_control, only: associate_node
        use ast_nodes_bounds, only: array_slice_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(expression_visitor_t), intent(in) :: visitor
        class(*), intent(inout), optional :: user_data

        type :: visit_stack_item_t
            integer :: node_index = 0
        end type visit_stack_item_t

        type(visit_stack_item_t), allocatable :: stack(:)
        character(len=:), allocatable :: node_type
        integer :: top, i, capacity
        integer :: current_index

        if (.not. associated(visitor%visit)) return
        if (.not. arena%has_node_at(root_index)) return

        capacity = 128
        allocate (stack(capacity))
        top = 1
        stack(top)%node_index = root_index

        do while (top > 0)
            current_index = stack(top)%node_index
            top = top - 1

            if (.not. arena%has_node_at(current_index)) cycle

            node_type = arena%entries(current_index)%node_type
            call visitor%visit(arena, current_index, node_type, user_data)

            select case (node_type)
            case ("binary_op")
                select type (node => arena%entries(current_index)%node)
                type is (binary_op_node)
                    call push(node%right_index)
                    call push(node%left_index)
                end select

            case ("call_or_subscript")
                select type (node => arena%entries(current_index)%node)
                type is (call_or_subscript_node)
                    if (allocated(node%arg_indices)) then
                        do i = size(node%arg_indices), 1, -1
                            call push(node%arg_indices(i))
                        end do
                    end if
                end select

            case ("array_slice")
                select type (node => arena%entries(current_index)%node)
                type is (array_slice_node)
                    do i = node%num_dimensions, 1, -1
                        call push(node%bounds_indices(i))
                    end do
                    call push(node%array_index)
                end select

            case ("component_access")
                select type (node => arena%entries(current_index)%node)
                type is (component_access_node)
                    call push(node%base_expr_index)
                end select

            case ("associate")
                select type (node => arena%entries(current_index)%node)
                type is (associate_node)
                    if (allocated(node%body_indices)) then
                        do i = size(node%body_indices), 1, -1
                            call push(node%body_indices(i))
                        end do
                    end if
                    if (allocated(node%associations)) then
                        do i = size(node%associations), 1, -1
                            call push(node%associations(i)%expr_index)
                        end do
                    end if
                end select
            end select
        end do

    contains
        subroutine push(idx)
            integer, intent(in) :: idx
            type(visit_stack_item_t), allocatable :: tmp(:)

            if (idx <= 0) return
            if (top + 1 > capacity) then
                capacity = capacity * 2
                allocate (tmp(capacity))
                tmp(1:top) = stack(1:top)
                call move_alloc(tmp, stack)
            end if
            top = top + 1
            stack(top)%node_index = idx
        end subroutine push
    end subroutine visit_expression_nodes

    ! Check if a specific variable is used in an expression
    function is_variable_used_in_expression(arena, expr_index, var_name) result(used)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: var_name
        logical :: used

        type(variable_usage_info_t) :: info
        integer :: i

        used = .false.
        info = get_variables_in_expression(arena, expr_index)

        if (allocated(info%variable_names)) then
            do i = 1, size(info%variable_names)
                if (info%variable_names(i) == var_name) then
                    used = .true.
                    exit
                end if
            end do
        end if
    end function is_variable_used_in_expression

    ! Count how many times a variable is used in an expression
    function count_variable_usage(arena, expr_index, var_name) result(count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: var_name
        integer :: count

        type(variable_usage_info_t) :: info
        integer :: i

        count = 0
        info = get_variables_in_expression(arena, expr_index)

        if (allocated(info%variable_names)) then
            do i = 1, size(info%variable_names)
                if (info%variable_names(i) == var_name) then
                    count = info%usage_counts(i)
                    exit
                end if
            end do
        end if
    end function count_variable_usage

end module variable_usage_tracker_module
