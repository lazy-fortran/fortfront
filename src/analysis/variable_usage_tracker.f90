module variable_usage_tracker_module
    use ast_core
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
        
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
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
            allocate(character(len=0) :: identifiers(0))
        end if
    end function get_identifiers_in_subtree

    ! Visit all expression nodes with a visitor function
    subroutine visit_expression_nodes(arena, root_index, visitor, user_data)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(expression_visitor_t), intent(in) :: visitor
        class(*), intent(inout), optional :: user_data
        
        if (.not. associated(visitor%visit)) return
        
        call visit_nodes_recursive(arena, root_index, visitor, user_data)
    end subroutine visit_expression_nodes

    ! Recursively visit nodes
    recursive subroutine visit_nodes_recursive(arena, node_index, visitor, user_data)
        use ast_nodes_control, only: associate_node
        use ast_nodes_bounds, only: array_slice_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(expression_visitor_t), intent(in) :: visitor
        class(*), intent(inout), optional :: user_data
        
        character(len=:), allocatable :: node_type
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        node_type = arena%entries(node_index)%node_type
        
        ! Visit this node
        call visitor%visit(arena, node_index, node_type, user_data)
        
        ! Recursively visit children (same logic as collect_identifiers_recursive)
        select case (node_type)
        case ("binary_op")
            select type (node => arena%entries(node_index)%node)
            type is (binary_op_node)
                if (node%left_index > 0) then
                    call visit_nodes_recursive(arena, node%left_index, visitor, user_data)
                end if
                if (node%right_index > 0) then
                    call visit_nodes_recursive(arena, node%right_index, visitor, user_data)
                end if
            end select
        case ("call_or_subscript")
            select type (node => arena%entries(node_index)%node)
            type is (call_or_subscript_node)
                ! The function/array name is stored as a string, not an index
                ! so we only visit the arguments/subscripts
                if (allocated(node%arg_indices)) then
                    do i = 1, size(node%arg_indices)
                        if (node%arg_indices(i) > 0) then
                            call visit_nodes_recursive(arena, node%arg_indices(i), visitor, user_data)
                        end if
                    end do
                end if
            end select
        case ("array_slice")
            select type (node => arena%entries(node_index)%node)
            type is (array_slice_node)
                ! Process array name
                if (node%array_index > 0) then
                    call visit_nodes_recursive(arena, node%array_index, visitor, user_data)
                end if
                
                ! Process slice bounds
                do i = 1, node%num_dimensions
                    if (node%bounds_indices(i) > 0) then
                        call visit_nodes_recursive(arena, node%bounds_indices(i), visitor, user_data)
                    end if
                end do
            end select
        case ("component_access")
            select type (node => arena%entries(node_index)%node)
            type is (component_access_node)
                ! Process the base expression (structure/derived type)
                if (node%base_expr_index > 0) then
                    call visit_nodes_recursive(arena, node%base_expr_index, visitor, user_data)
                end if
            end select
        case ("associate")
            select type (node => arena%entries(node_index)%node)
            type is (associate_node)
                ! Visit association expressions
                if (allocated(node%associations)) then
                    do i = 1, size(node%associations)
                        if (node%associations(i)%expr_index > 0) then
                            call visit_nodes_recursive(arena, node%associations(i)%expr_index, visitor, user_data)
                        end if
                    end do
                end if
                
                ! Visit body statements
                if (allocated(node%body_indices)) then
                    do i = 1, size(node%body_indices)
                        if (node%body_indices(i) > 0) then
                            call visit_nodes_recursive(arena, node%body_indices(i), visitor, user_data)
                        end if
                    end do
                end if
            end select
        end select
    end subroutine visit_nodes_recursive

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