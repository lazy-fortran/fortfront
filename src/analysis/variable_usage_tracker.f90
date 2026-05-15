! @slow-path
module variable_usage_tracker_module
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core
    use ast_arena_modern
    use variable_usage_core_module
    use variable_usage_dispatcher_module
    use ast_nodes_core, only: assignment_node, binary_op_node, &
                              call_or_subscript_node, component_access_node, &
                              identifier_node, program_node
    use ast_nodes_control, only: block_construct_node, do_loop_node, if_node
    use ast_nodes_data, only: declaration_node, module_node
    use ast_nodes_io, only: print_statement_node
    use ast_nodes_procedure, only: function_def_node, subroutine_call_node, &
                                   subroutine_def_node
    implicit none
    private

    type :: scoped_variable_usage_t
        character(len=:), allocatable :: name
        integer :: node_index = 0
        integer :: scope_id = 0
        integer :: parent_scope_id = 0
        integer :: scope_depth = 0
        logical :: is_declaration = .false.
    end type scoped_variable_usage_t

    ! Re-export types from core module
    public :: variable_usage_info_t, expression_visitor_t
    public :: scoped_variable_usage_t

    ! Public procedures
    public :: create_variable_usage_info, get_variables_in_expression
    public :: get_identifiers_in_subtree, visit_expression_nodes
    public :: is_variable_used_in_expression, count_variable_usage
    public :: get_scoped_variable_usages

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

    function get_scoped_variable_usages(arena, root_index) result(usages)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(scoped_variable_usage_t), allocatable :: usages(:)

        integer :: count
        integer :: capacity
        integer :: next_scope_id

        count = 0
        capacity = 16
        next_scope_id = 0
        allocate (usages(capacity))

        if (arena%has_node_at(root_index)) then
            call collect_scoped_usage(arena, root_index, 0, 0, 0, &
                                      next_scope_id, usages, count, capacity)
        end if

        call resize_scoped_usages(usages, count)
    end function get_scoped_variable_usages

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

    recursive subroutine collect_scoped_usage(arena, node_index, scope_id, &
                                             parent_scope_id, scope_depth, &
                                             next_scope_id, usages, count, &
                                             capacity)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, intent(in) :: scope_id
        integer, intent(in) :: parent_scope_id
        integer, intent(in) :: scope_depth
        integer, intent(inout) :: next_scope_id
        type(scoped_variable_usage_t), allocatable, intent(inout) :: usages(:)
        integer, intent(inout) :: count
        integer, intent(inout) :: capacity

        integer :: current_scope_id
        integer :: current_parent_scope_id
        integer :: current_scope_depth

        if (.not. arena%has_node_at(node_index)) return

        current_scope_id = scope_id
        current_parent_scope_id = parent_scope_id
        current_scope_depth = scope_depth

        if (node_starts_scope(arena, node_index)) then
            next_scope_id = next_scope_id + 1
            current_parent_scope_id = scope_id
            current_scope_id = next_scope_id
            current_scope_depth = scope_depth + 1
        end if

        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            call collect_index_list(node%body_indices)

        type is (module_node)
            call collect_index_list(node%declaration_indices)
            call collect_index_list(node%procedure_indices)

        type is (subroutine_def_node)
            call collect_index_list(node%param_indices)
            call collect_index_list(node%body_indices)

        type is (function_def_node)
            call collect_index_list(node%param_indices)
            call collect_index_list(node%body_indices)

        type is (block_construct_node)
            call collect_index_list(node%body_indices)

        type is (declaration_node)
            call append_declaration_names(node, node_index, current_scope_id, &
                                          current_parent_scope_id, &
                                          current_scope_depth, usages, count, &
                                          capacity)
            if (node%has_initializer) then
                call collect_child(node%initializer_index)
            end if
            call collect_index_list(node%dimension_indices)

        type is (identifier_node)
            if (allocated(node%name)) then
                call append_scoped_usage(node%name, node_index, current_scope_id, &
                                         current_parent_scope_id, &
                                         current_scope_depth, .false., usages, &
                                         count, capacity)
            end if

        type is (assignment_node)
            call collect_child(node%target_index)
            call collect_child(node%value_index)

        type is (binary_op_node)
            call collect_child(node%left_index)
            call collect_child(node%right_index)

        type is (call_or_subscript_node)
            if (allocated(node%name) .and. .not. node%is_array_access) then
                call append_scoped_usage(node%name, node_index, current_scope_id, &
                                         current_parent_scope_id, &
                                         current_scope_depth, .false., usages, &
                                         count, capacity)
            end if
            call collect_child(node%base_expr_index)
            call collect_index_list(node%arg_indices)

        type is (component_access_node)
            call collect_child(node%base_expr_index)

        type is (if_node)
            call collect_child(node%condition_index)
            call collect_index_list(node%then_body_indices)
            call collect_index_list(node%else_body_indices)

        type is (do_loop_node)
            if (allocated(node%var_name)) then
                call append_scoped_usage(node%var_name, node_index, current_scope_id, &
                                         current_parent_scope_id, &
                                         current_scope_depth, .false., usages, &
                                         count, capacity)
            end if
            call collect_child(node%start_expr_index)
            call collect_child(node%end_expr_index)
            call collect_child(node%step_expr_index)
            call collect_index_list(node%body_indices)

        type is (print_statement_node)
            call collect_index_list(node%expression_indices)

        type is (subroutine_call_node)
            if (allocated(node%name)) then
                call append_scoped_usage(node%name, node_index, current_scope_id, &
                                         current_parent_scope_id, &
                                         current_scope_depth, .false., usages, &
                                         count, capacity)
            end if
            call collect_index_list(node%arg_indices)
        end select

    contains
        subroutine collect_child(child_index)
            integer, intent(in) :: child_index

            if (child_index <= 0) return
            call collect_scoped_usage(arena, child_index, current_scope_id, &
                                      current_parent_scope_id, &
                                      current_scope_depth, next_scope_id, &
                                      usages, count, capacity)
        end subroutine collect_child

        subroutine collect_index_list(indices)
            integer, intent(in), optional :: indices(:)
            integer :: i

            if (.not. present(indices)) return
            do i = 1, size(indices)
                call collect_child(indices(i))
            end do
        end subroutine collect_index_list
    end subroutine collect_scoped_usage

    logical function node_starts_scope(arena, node_index) result(starts_scope)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index

        starts_scope = .false.
        if (.not. arena%has_node_at(node_index)) return

        select case (arena%entries(node_index)%node_type)
        case ("program", "module", "subroutine_def", "function_def", &
              "block_construct")
            starts_scope = .true.
        end select
    end function node_starts_scope

    subroutine append_declaration_names(node, node_index, scope_id, &
                                        parent_scope_id, scope_depth, usages, &
                                        count, capacity)
        type(declaration_node), intent(in) :: node
        integer, intent(in) :: node_index
        integer, intent(in) :: scope_id
        integer, intent(in) :: parent_scope_id
        integer, intent(in) :: scope_depth
        type(scoped_variable_usage_t), allocatable, intent(inout) :: usages(:)
        integer, intent(inout) :: count
        integer, intent(inout) :: capacity
        integer :: i

        if (node%is_multi_declaration .and. allocated(node%var_names)) then
            do i = 1, size(node%var_names)
                call append_scoped_usage(node%var_names(i), node_index, scope_id, &
                                         parent_scope_id, scope_depth, .true., &
                                         usages, count, capacity)
            end do
        else if (allocated(node%var_name)) then
            call append_scoped_usage(node%var_name, node_index, scope_id, &
                                     parent_scope_id, scope_depth, .true., &
                                     usages, count, capacity)
        end if
    end subroutine append_declaration_names

    subroutine append_scoped_usage(name, node_index, scope_id, parent_scope_id, &
                                  scope_depth, is_declaration, usages, count, &
                                  capacity)
        character(len=*), intent(in) :: name
        integer, intent(in) :: node_index
        integer, intent(in) :: scope_id
        integer, intent(in) :: parent_scope_id
        integer, intent(in) :: scope_depth
        logical, intent(in) :: is_declaration
        type(scoped_variable_usage_t), allocatable, intent(inout) :: usages(:)
        integer, intent(inout) :: count
        integer, intent(inout) :: capacity
        type(scoped_variable_usage_t), allocatable :: tmp(:)

        if (len_trim(name) == 0) return

        if (count >= capacity) then
            capacity = max(capacity * 2, capacity + 16)
            allocate (tmp(capacity))
            if (count > 0) tmp(1:count) = usages(1:count)
            call move_alloc(tmp, usages)
        end if

        count = count + 1
        usages(count)%name = trim(name)
        usages(count)%node_index = node_index
        usages(count)%scope_id = scope_id
        usages(count)%parent_scope_id = parent_scope_id
        usages(count)%scope_depth = scope_depth
        usages(count)%is_declaration = is_declaration
    end subroutine append_scoped_usage

    subroutine resize_scoped_usages(usages, count)
        type(scoped_variable_usage_t), allocatable, intent(inout) :: usages(:)
        integer, intent(in) :: count
        type(scoped_variable_usage_t), allocatable :: tmp(:)

        allocate (tmp(count))
        if (count > 0) tmp = usages(1:count)
        call move_alloc(tmp, usages)
    end subroutine resize_scoped_usages

end module variable_usage_tracker_module
