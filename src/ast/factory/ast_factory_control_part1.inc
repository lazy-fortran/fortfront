
    ! Create if statement node and add to stack
    function push_if(arena, condition_index, then_body_indices, elseif_indices, &
                     else_body_indices, &
                     line, column, parent_index) result(if_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: then_body_indices(:)
        integer, intent(in), optional :: elseif_indices(:)
        integer, intent(in), optional :: else_body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: if_index
        type(if_node) :: if_stmt
        integer :: i

        if_stmt%uid = generate_uid()

        ! Set condition index
        if (condition_index > 0 .and. condition_index <= arena%size) then
            if_stmt%condition_index = condition_index
        end if

        ! Set then body indices
        if (present(then_body_indices)) then
            if (size(then_body_indices) > 0) then
                if_stmt%then_body_indices = then_body_indices
            end if
        end if

        ! Set else body indices
        if (present(else_body_indices)) then
            if (size(else_body_indices) > 0) then
                if_stmt%else_body_indices = else_body_indices
            end if
        end if

        ! Handle elseif blocks
        if (present(elseif_indices)) then
            if (size(elseif_indices) > 0) then
                ! For now, treat elseif_indices as pairs: &
                ! condition, body, condition, body, ...
                ! Each pair becomes one elseif_wrapper
                if (mod(size(elseif_indices), 2) == 0) then
                    allocate (if_stmt%elseif_blocks(size(elseif_indices) / 2))
                    do i = 1, size(elseif_indices) / 2
                        if_stmt%elseif_blocks(i)%condition_index = &
                            elseif_indices(2 * i - 1)
                        if_stmt%elseif_blocks(i)%body_indices = [elseif_indices(2 * i)]
                    end do
                end if
            end if
        end if

        if (present(line)) if_stmt%line = line
        if (present(column)) if_stmt%column = column

        call arena%push(if_stmt, "if_statement", parent_index)
        if_index = arena%size

        ! Link children to this parent for AST traversal
        if (condition_index > 0) then
            call link_children_to_parent(arena, if_index, [condition_index])
        end if
        if (present(then_body_indices)) then
            if (size(then_body_indices) > 0) then
                call link_children_to_parent(arena, if_index, then_body_indices)
            end if
        end if
        if (present(elseif_indices)) then
            if (size(elseif_indices) > 0) then
                call link_children_to_parent(arena, if_index, elseif_indices)
            end if
        end if
        if (present(else_body_indices)) then
            if (size(else_body_indices) > 0) then
                call link_children_to_parent(arena, if_index, else_body_indices)
            end if
        end if
    end function push_if

    ! Create do loop node and add to stack
function push_do_loop(arena, var_name, start_index, end_index, step_index, &
                           body_indices, &
                           loop_label, is_concurrent, line, column, parent_index, &
                           type_spec) &
        result(loop_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_index, end_index
        integer, intent(in), optional :: step_index
        integer, intent(in), optional :: body_indices(:)
        character(len=*), intent(in), optional :: loop_label
        logical, intent(in), optional :: is_concurrent
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: type_spec
        integer :: loop_index
        type(do_loop_node) :: loop_node

        loop_node%uid = generate_uid()

        loop_node%var_name = var_name
        if (present(loop_label)) loop_node%label = loop_label
        if (present(is_concurrent)) loop_node%is_concurrent = is_concurrent
        if (present(type_spec) .and. len_trim(type_spec) > 0) loop_node%type_spec = type_spec

        ! Set start and end expression indices
        if (start_index > 0 .and. start_index <= arena%size) then
            loop_node%start_expr_index = start_index
        end if

        if (end_index > 0 .and. end_index <= arena%size) then
            loop_node%end_expr_index = end_index
        end if

        ! Set optional step expression index
        if (present(step_index)) then
            if (step_index > 0) then
                loop_node%step_expr_index = step_index
            end if
        end if

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                loop_node%body_indices = body_indices
            end if
        end if

        if (present(line)) loop_node%line = line
        if (present(column)) loop_node%column = column

        call arena%push(loop_node, "do_loop", parent_index)
        loop_index = arena%size

        ! Link children to this parent for AST traversal
        if (start_index > 0) then
            call link_children_to_parent(arena, loop_index, [start_index])
        end if
        if (end_index > 0) then
            call link_children_to_parent(arena, loop_index, [end_index])
        end if
        if (present(step_index)) then
            if (step_index > 0) then
                call link_children_to_parent(arena, loop_index, [step_index])
            end if
        end if
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, loop_index, body_indices)
            end if
        end if
    end function push_do_loop

    ! Create do while loop node and add to stack
    function push_do_while(arena, condition_index, body_indices, loop_label, &
                           line, column, parent_index) result(while_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: body_indices(:)
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: while_index
        type(do_while_node) :: while_node

        while_node%uid = generate_uid()

        ! Set label if provided
        if (present(loop_label)) while_node%label = loop_label

        ! Set condition index
        if (condition_index > 0 .and. condition_index <= arena%size) then
            while_node%condition_index = condition_index
        end if

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                while_node%body_indices = body_indices
            end if
        end if

        if (present(line)) while_node%line = line
        if (present(column)) while_node%column = column

        call arena%push(while_node, "do_while", parent_index)
        while_index = arena%size

        ! Link children to this parent for AST traversal
        if (condition_index > 0) then
            call link_children_to_parent(arena, while_index, [condition_index])
        end if
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, while_index, body_indices)
            end if
        end if
    end function push_do_while

    ! Create ASSOCIATE construct node and add to stack
    function push_associate(arena, associations, body_indices, line, column, &
                            parent_index) result(assoc_index)
        use ast_nodes_control, only: associate_node, association_t
        type(ast_arena_t), intent(inout) :: arena
        type(association_t), intent(in) :: associations(:)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: assoc_index
        type(associate_node) :: assoc

        assoc%uid = generate_uid()

        ! Set associations
        if (size(associations) > 0) then
            assoc%associations = associations
        end if

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                assoc%body_indices = body_indices
            end if
        end if

        if (present(line)) assoc%line = line
        if (present(column)) assoc%column = column

        call arena%push(assoc, "associate", parent_index)
        assoc_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, assoc_index, body_indices)
            end if
        end if
    end function push_associate

    function push_block_construct(arena, body_indices, line, column, &
                                  parent_index) result(block_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: block_index
        type(block_construct_node) :: block_construct

        block_construct%uid = generate_uid()

        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                block_construct%body_indices = body_indices
            end if
        end if

        if (present(line)) block_construct%line = line
        if (present(column)) block_construct%column = column

        call arena%push(block_construct, "block_construct", parent_index)
        block_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, block_index, body_indices)
            end if
        end if
    end function push_block_construct

    ! Create forall construct node and add to stack
    function push_forall(arena, index_var, start_index, end_index, step_index, &
                         mask_index, body_indices, line, column, parent_index, &
                         index_vars_all, start_indices_all, end_indices_all, &
                         stride_indices_all) result(forall_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: index_var
        integer, intent(in) :: start_index, end_index
        integer, intent(in), optional :: step_index, mask_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        character(len=*), intent(in), optional :: index_vars_all(:)
        integer, intent(in), optional :: start_indices_all(:)
        integer, intent(in), optional :: end_indices_all(:)
        integer, intent(in), optional :: stride_indices_all(:)
        integer :: forall_index
        type(forall_node) :: forall_stmt
        integer :: i, index_var_len, num_indices, max_index_len, stride_val
        logical :: use_multi
        integer, allocatable :: lower_vals(:), upper_vals(:), stride_vals(:)
        type(result_t) :: validation

        forall_stmt%uid = generate_uid()

        ! Validate arena is initialized
        validation = validate_arena(arena, "push_forall")
        if (validation%is_failure()) then
            write (error_unit, '(A)') "ERROR [ast_factory_control]: " // &
                validation%get_full_message()
            forall_index = 0
            return
        end if

        use_multi = present(index_vars_all) .and. present(start_indices_all) .and. &
                    present(end_indices_all)

        if (use_multi) then
            num_indices = size(index_vars_all)
            if (num_indices <= 0) then
                write (error_unit, '(A)') &
                    "ERROR [ast_factory_control]: FORALL requires at least one index"
                forall_index = 0
                return
            end if
            if (size(start_indices_all) /= num_indices .or. &
                size(end_indices_all) /= num_indices) then
                write(error_unit, '(A)') "ERROR [ast_factory_control]: FORALL index arrays must be the same size"
                forall_index = 0
                return
            end if
            if (present(stride_indices_all)) then
                if (size(stride_indices_all) /= num_indices) then
                    write (error_unit, '(A)') &
               "ERROR [ast_factory_control]: FORALL stride array must match index count"
                    forall_index = 0
                    return
                end if
            end if
        else
            num_indices = 1
        end if

        allocate (lower_vals(num_indices))
        allocate (upper_vals(num_indices))
        allocate (stride_vals(num_indices))

        max_index_len = 0
        do i = 1, num_indices
            if (use_multi) then
                index_var_len = len_trim(index_vars_all(i))
                lower_vals(i) = start_indices_all(i)
                upper_vals(i) = end_indices_all(i)
                if (present(stride_indices_all)) then
                    stride_val = stride_indices_all(i)
                else
                    stride_val = 0
                end if
            else
                index_var_len = len_trim(index_var)
                lower_vals(i) = start_index
                upper_vals(i) = end_index
                if (present(step_index)) then
                    stride_val = step_index
                else
                    stride_val = 0
                end if
            end if

            if (index_var_len == 0) then
                write (error_unit, '(A)') &
               "ERROR [ast_factory_control]: FORALL index variable name cannot be empty"
                forall_index = 0
                return
            end if
            if (index_var_len > MAX_INDEX_NAME_LENGTH) then
                write (error_unit, '(A)') &
        "ERROR [ast_factory_control]: FORALL index variable name exceeds maximum length"
                forall_index = 0
                return
            end if

            max_index_len = max(max_index_len, index_var_len)

            validation = validate_node_index(arena, lower_vals(i), "push_forall start")
            if (validation%is_failure()) then
                write (error_unit, '(A)') "ERROR [ast_factory_control]: " // &
                    validation%get_full_message()
                forall_index = 0
                return
            end if

            validation = validate_node_index(arena, upper_vals(i), "push_forall end")
            if (validation%is_failure()) then
                write (error_unit, '(A)') "ERROR [ast_factory_control]: " // &
                    validation%get_full_message()
                forall_index = 0
                return
            end if

            if (stride_val > 0) then
                validation = validate_node_index(arena, stride_val, "push_forall step")
                if (validation%is_failure()) then
                    write (error_unit, '(A)') "ERROR [ast_factory_control]: " // &
                        validation%get_full_message()
                    forall_index = 0
                    return
                end if
                stride_vals(i) = stride_val
            else
                stride_vals(i) = 0
            end if
        end do

        if (max_index_len <= 0) max_index_len = 1

        ! Validate optional mask index
        if (present(mask_index)) then
            if (mask_index > 0) then
                validation = validate_node_index(arena, mask_index, "push_forall mask")
                if (validation%is_failure()) then
                    write (error_unit, '(A)') "ERROR [ast_factory_control]: " // &
                        validation%get_full_message()
                    forall_index = 0
                    return
                end if
            end if
        end if

        ! Validate body indices
        if (present(body_indices)) then
            do i = 1, size(body_indices)
                validation = validate_node_index(arena, body_indices(i), &
                                                 "push_forall body")
                if (validation%is_failure()) then
                    write (error_unit, '(A)') "ERROR [ast_factory_control]: " // &
                        validation%get_full_message()
                    forall_index = 0
                    return
                end if
            end do
        end if

        forall_stmt%num_indices = num_indices
        allocate (character(len=max_index_len) :: forall_stmt%index_names(num_indices))
        allocate (forall_stmt%lower_bound_indices(num_indices))
        allocate (forall_stmt%upper_bound_indices(num_indices))
        allocate (forall_stmt%stride_indices(num_indices))

        do i = 1, num_indices
            if (use_multi) then
                forall_stmt%index_names(i) = trim(index_vars_all(i))
            else
                forall_stmt%index_names(i) = trim(index_var)
            end if
            forall_stmt%lower_bound_indices(i) = lower_vals(i)
            forall_stmt%upper_bound_indices(i) = upper_vals(i)
            forall_stmt%stride_indices(i) = stride_vals(i)
        end do

        ! Set optional mask expression index
        if (present(mask_index)) then
            if (mask_index > 0) then
                forall_stmt%has_mask = .true.
                forall_stmt%mask_expr_index = mask_index
            else
                forall_stmt%has_mask = .false.
            end if
        else
            forall_stmt%has_mask = .false.
        end if

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                forall_stmt%body_indices = body_indices
            end if
        end if

        if (present(line)) forall_stmt%line = line
        if (present(column)) forall_stmt%column = column

        if (present(parent_index)) then
            call arena%push(forall_stmt, "forall", parent_index)
        else
            call arena%push(forall_stmt, "forall")
        end if
        forall_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, forall_index, body_indices)
            end if
        end if
    end function push_forall

    ! Create select case node and add to stack
    function push_select_case(arena, selector_index, case_indices, line, &
                              column, parent_index) result(select_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: selector_index
        integer, intent(in), optional :: case_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: select_index
        type(select_case_node) :: select_node

        select_node%uid = generate_uid()

        ! Set selector expression index
        if (selector_index > 0 .and. selector_index <= arena%size) then
            select_node%selector_index = selector_index
        end if

        ! Set case indices
        if (present(case_indices)) then
            if (size(case_indices) > 0) then
                select_node%case_indices = case_indices
            end if
        end if

        if (present(line)) select_node%line = line
        if (present(column)) select_node%column = column

        call arena%push(select_node, "select_case", parent_index)
        select_index = arena%size

        ! Link children to this parent for AST traversal
        if (selector_index > 0) then
            call link_children_to_parent(arena, select_index, [selector_index])
        end if
        if (present(case_indices)) then
            if (size(case_indices) > 0) then
                call link_children_to_parent(arena, select_index, case_indices)
            end if
        end if

    end function push_select_case

    ! Create select case node with default case and add to stack
    function push_select_case_with_default(arena, selector_index, case_indices, &
                                           default_index, &
                                           line, column, parent_index) &
        result(select_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: selector_index
        integer, intent(in), optional :: case_indices(:)
        integer, intent(in) :: default_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: select_index
        type(select_case_node) :: select_node

        select_node%uid = generate_uid()

        ! Set selector expression index
        if (selector_index > 0 .and. selector_index <= arena%size) then
            select_node%selector_index = selector_index
        end if

        ! Set case indices
        if (present(case_indices)) then
            if (size(case_indices) > 0) then
                select_node%case_indices = case_indices
            end if
        end if

        ! Set default case index
        if (default_index > 0 .and. default_index <= arena%size) then
            select_node%default_index = default_index
        end if

        if (present(line)) select_node%line = line
        if (present(column)) select_node%column = column

        call arena%push(select_node, "select_case", parent_index)
        select_index = arena%size

        ! Link children to this parent for AST traversal
        if (selector_index > 0) then
            call link_children_to_parent(arena, select_index, [selector_index])
        end if
        if (present(case_indices)) then
            if (size(case_indices) > 0) then
                call link_children_to_parent(arena, select_index, case_indices)
            end if
        end if
        if (default_index > 0) then
            call link_children_to_parent(arena, select_index, [default_index])
        end if

    end function push_select_case_with_default

    ! Create case block node and add to stack
    function push_case_block(arena, value_indices, body_indices, line, column, &
                             parent_index) result(case_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: value_indices(:)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: case_index
        type(case_block_node) :: case_node

        case_node%uid = generate_uid()

        ! Set case values
        if (size(value_indices) > 0) then
            case_node%value_indices = value_indices
        end if

        ! Set case body
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                case_node%body_indices = body_indices
            end if
        end if

        if (present(line)) case_node%line = line
        if (present(column)) case_node%column = column

        call arena%push(case_node, "case_block", parent_index)
        case_index = arena%size

        ! Link children to this parent for AST traversal
        if (size(value_indices) > 0) then
            call link_children_to_parent(arena, case_index, value_indices)
        end if
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, case_index, body_indices)
            end if
        end if

    end function push_case_block

    ! Create case range node and add to stack
    function push_case_range(arena, start_value, end_value, line, column, &
                             parent_index) result(range_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: start_value, end_value
        integer, intent(in), optional :: line, column, parent_index
        integer :: range_index
        type(case_range_node) :: range_node

        range_node%uid = generate_uid()

        range_node%start_value = start_value
        range_node%end_value = end_value

        if (present(line)) range_node%line = line
        if (present(column)) range_node%column = column

        call arena%push(range_node, "case_range", parent_index)
        range_index = arena%size
    end function push_case_range

    ! Create case default node and add to stack
    function push_case_default(arena, body_indices, line, column, &
                               parent_index) result(default_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: default_index
        type(case_default_node) :: default_node

        default_node%uid = generate_uid()

        ! Set default case body
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                default_node%body_indices = body_indices
            end if
        end if

        if (present(line)) default_node%line = line
        if (present(column)) default_node%column = column

        call arena%push(default_node, "case_default", parent_index)
        default_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, default_index, body_indices)
            end if
        end if
    end function push_case_default

    ! Create SELECT TYPE node without default and add to stack
    function push_select_type(arena, selector_index, guard_indices, &
                              line, column, parent_index) result(select_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: selector_index
        integer, intent(in), optional :: guard_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: select_index

        type(select_type_node) :: select_node

        select_node%uid = generate_uid()

        ! Set selector expression index
        if (selector_index > 0 .and. selector_index <= arena%size) then
            select_node%selector_index = selector_index
        end if

        ! Set guard indices
        if (present(guard_indices)) then
            if (size(guard_indices) > 0) then
                select_node%guard_indices = guard_indices
            end if
        end if

        ! Set location information
        if (present(line)) select_node%line = line
        if (present(column)) select_node%column = column

        ! Add node to arena
        if (present(parent_index)) then
            call arena%push(select_node, "select_type", parent_index)
        else
            call arena%push(select_node, "select_type")
        end if
        select_index = arena%size

        ! Link children to this parent for AST traversal
        if (selector_index > 0) then
            call link_children_to_parent(arena, select_index, [selector_index])
        end if
        if (present(guard_indices)) then
            if (size(guard_indices) > 0) then
                call link_children_to_parent(arena, select_index, guard_indices)
            end if
        end if
    end function push_select_type

    ! Create SELECT TYPE node with default and add to stack
    function push_select_type_with_default(arena, selector_index, &
                                           guard_indices, default_index, &
                                           line, column, parent_index) &
        result(select_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: selector_index
        integer, intent(in), optional :: guard_indices(:)
        integer, intent(in) :: default_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: select_index

        type(select_type_node) :: select_node

        select_node%uid = generate_uid()

        ! Set selector expression index
        if (selector_index > 0 .and. selector_index <= arena%size) then
            select_node%selector_index = selector_index
        end if

        ! Set guard indices
        if (present(guard_indices)) then
            if (size(guard_indices) > 0) then
                select_node%guard_indices = guard_indices
            end if
        end if

        ! Set default guard index
        if (default_index > 0 .and. default_index <= arena%size) then
            select_node%default_index = default_index
        end if

        ! Set location information
        if (present(line)) select_node%line = line
        if (present(column)) select_node%column = column

        ! Add node to arena
        if (present(parent_index)) then
            call arena%push(select_node, "select_type", parent_index)
        else
            call arena%push(select_node, "select_type")
        end if
        select_index = arena%size

        ! Link children to this parent for AST traversal
        if (selector_index > 0) then
            call link_children_to_parent(arena, select_index, [selector_index])
        end if
        if (present(guard_indices)) then
            if (size(guard_indices) > 0) then
                call link_children_to_parent(arena, select_index, guard_indices)
            end if
        end if
        if (default_index > 0) then
            call link_children_to_parent(arena, select_index, [default_index])
        end if
    end function push_select_type_with_default

    ! Create type guard block node and add to stack
    function push_type_guard_block(arena, guard_type, type_name_index, &
                                   body_indices, line, column, parent_index) &
        result(guard_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: guard_type
        integer, intent(in) :: type_name_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: guard_index

        type(type_guard_block_node) :: guard_node

        guard_node%uid = generate_uid()
        guard_node%guard_type = guard_type
        guard_node%type_name_index = type_name_index

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                guard_node%body_indices = body_indices
            end if
        end if

        ! Set location information
        if (present(line)) guard_node%line = line
        if (present(column)) guard_node%column = column

        ! Add node to arena
        if (present(parent_index)) then
            call arena%push(guard_node, "type_guard_block", parent_index)
        else
            call arena%push(guard_node, "type_guard_block")
        end if
        guard_index = arena%size

        ! Link children to this parent for AST traversal
        if (type_name_index > 0) then
            call link_children_to_parent(arena, guard_index, [type_name_index])
        end if
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, guard_index, body_indices)
            end if
        end if
    end function push_type_guard_block

    ! Create SELECT RANK node and add to stack
    function push_select_rank(arena, selector_index, rank_indices, &
                              line, column, parent_index) result(select_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: selector_index
        integer, intent(in), optional :: rank_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: select_index

        type(select_rank_node) :: select_node

        select_node%uid = generate_uid()

        ! Set selector expression index
        if (selector_index > 0 .and. selector_index <= arena%size) then
            select_node%selector_index = selector_index
        end if

        ! Set rank indices
        if (present(rank_indices)) then
            if (size(rank_indices) > 0) then
                select_node%rank_indices = rank_indices
            end if
        end if

        ! Set location information
        if (present(line)) select_node%line = line
        if (present(column)) select_node%column = column

        ! Add node to arena
        if (present(parent_index)) then
            call arena%push(select_node, "select_rank", parent_index)
        else
            call arena%push(select_node, "select_rank")
        end if
        select_index = arena%size

        ! Link children to this parent for AST traversal
        if (selector_index > 0) then
            call link_children_to_parent(arena, select_index, [selector_index])
        end if
        if (present(rank_indices)) then
            if (size(rank_indices) > 0) then
                call link_children_to_parent(arena, select_index, rank_indices)
            end if
        end if
    end function push_select_rank

    ! Create SELECT RANK node with default and add to stack
    function push_select_rank_with_default(arena, selector_index, &
                                           rank_indices, default_index, &
                                           line, column, parent_index) &
        result(select_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: selector_index
        integer, intent(in), optional :: rank_indices(:)
        integer, intent(in) :: default_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: select_index

        type(select_rank_node) :: select_node

        select_node%uid = generate_uid()

        ! Set selector expression index
        if (selector_index > 0 .and. selector_index <= arena%size) then
            select_node%selector_index = selector_index
        end if

        ! Set rank indices
        if (present(rank_indices)) then
            if (size(rank_indices) > 0) then
                select_node%rank_indices = rank_indices
            end if
        end if

        ! Set default rank index
        if (default_index > 0 .and. default_index <= arena%size) then
            select_node%default_index = default_index
        end if

        ! Set location information
        if (present(line)) select_node%line = line
        if (present(column)) select_node%column = column

        ! Add node to arena
        if (present(parent_index)) then
            call arena%push(select_node, "select_rank", parent_index)
        else
            call arena%push(select_node, "select_rank")
        end if
        select_index = arena%size

        ! Link children to this parent for AST traversal
        if (selector_index > 0) then
            call link_children_to_parent(arena, select_index, [selector_index])
        end if
        if (present(rank_indices)) then
            if (size(rank_indices) > 0) then
                call link_children_to_parent(arena, select_index, rank_indices)
            end if
        end if
        if (default_index > 0) then
            call link_children_to_parent(arena, select_index, [default_index])
        end if
    end function push_select_rank_with_default

    ! Create rank block node and add to stack
    function push_rank_block(arena, rank_value, body_indices, &
                             line, column, parent_index) result(rank_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: rank_value
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: rank_index

        type(rank_block_node) :: rank_node

        rank_node%uid = generate_uid()
        rank_node%rank_value = rank_value

        ! Set body indices
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                rank_node%body_indices = body_indices
            end if
        end if

        ! Set location information
        if (present(line)) rank_node%line = line
        if (present(column)) rank_node%column = column

        ! Add node to arena
        if (present(parent_index)) then
            call arena%push(rank_node, "rank_block", parent_index)
        else
            call arena%push(rank_node, "rank_block")
        end if
        rank_index = arena%size

        ! Link children to this parent for AST traversal
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                call link_children_to_parent(arena, rank_index, body_indices)
            end if
        end if
    end function push_rank_block
