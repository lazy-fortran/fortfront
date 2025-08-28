module ast_factory_control
    use ast_core
    use ast_factory_core, only: validate_arena, validate_node_index
    use ast_nodes_control, only: MAX_INDEX_NAME_LENGTH
    use error_handling, only: result_t, success_result, create_error_result
    implicit none
    private

    ! Public control flow node creation functions
    public :: push_if, push_do_loop, push_do_while, push_forall, push_select_case
    public :: push_associate
    public :: push_case_block, push_case_range, push_case_default, &
              push_select_case_with_default
    public :: push_where, push_where_construct, push_where_construct_with_elsewhere

contains

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
                    allocate (if_stmt%elseif_blocks(size(elseif_indices)/2))
                    do i = 1, size(elseif_indices)/2
                      if_stmt%elseif_blocks(i)%condition_index = elseif_indices(2*i - 1)
                        if_stmt%elseif_blocks(i)%body_indices = [elseif_indices(2*i)]
                    end do
                end if
            end if
        end if

        if (present(line)) if_stmt%line = line
        if (present(column)) if_stmt%column = column

        call arena%push(if_stmt, "if_statement", parent_index)
        if_index = arena%size
    end function push_if

    ! Create do loop node and add to stack
    function push_do_loop(arena, var_name, start_index, end_index, step_index, &
                         body_indices, &
                          loop_label, line, column, parent_index) result(loop_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_index, end_index
        integer, intent(in), optional :: step_index
        integer, intent(in), optional :: body_indices(:)
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column, parent_index
        integer :: loop_index
        type(do_loop_node) :: loop_node
        integer :: i

        loop_node%var_name = var_name
        if (present(loop_label)) loop_node%label = loop_label

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
    end function push_do_loop

    ! Create do while loop node and add to stack
    function push_do_while(arena, condition_index, body_indices, line, column, &
                          parent_index) result(while_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: while_index
        type(do_while_node) :: while_node
        integer :: i

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
    end function push_associate

    ! Create forall construct node and add to stack
    function push_forall(arena, index_var, start_index, end_index, step_index, &
              mask_index, body_indices, line, column, parent_index) result(forall_index)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: index_var
        integer, intent(in) :: start_index, end_index
        integer, intent(in), optional :: step_index, mask_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: forall_index
        type(forall_node) :: forall_stmt
        integer :: i, index_var_len

        type(result_t) :: validation

        ! Validate arena is initialized
        validation = validate_arena(arena, "push_forall")
        if (validation%is_failure()) then
            write(*, '(A)') "ERROR [ast_factory_control]: " // validation%get_full_message()
            forall_index = 0
            return
        end if

        ! Validate index variable name
        index_var_len = len_trim(index_var)
        if (index_var_len == 0) then
            write(*, '(A)') "ERROR [ast_factory_control]: FORALL index variable name cannot be empty"
            forall_index = 0
            return
        end if
        if (index_var_len > MAX_INDEX_NAME_LENGTH) then
            write(*, '(A)') "ERROR [ast_factory_control]: FORALL index variable name exceeds maximum length"
            forall_index = 0
            return
        end if

        ! Validate required indices
        validation = validate_node_index(arena, start_index, "push_forall start")
        if (validation%is_failure()) then
            write(*, '(A)') "ERROR [ast_factory_control]: " // validation%get_full_message()
            forall_index = 0
            return
        end if

        validation = validate_node_index(arena, end_index, "push_forall end")
        if (validation%is_failure()) then
            write(*, '(A)') "ERROR [ast_factory_control]: " // validation%get_full_message()
            forall_index = 0
            return
        end if

        ! Validate optional step index
        if (present(step_index)) then
            if (step_index > 0) then
                validation = validate_node_index(arena, step_index, "push_forall step")
                if (validation%is_failure()) then
                    write(*, '(A)') "ERROR [ast_factory_control]: " // validation%get_full_message()
                    forall_index = 0
                    return
                end if
            end if
        end if

        ! Validate optional mask index
        if (present(mask_index)) then
            if (mask_index > 0) then
                validation = validate_node_index(arena, mask_index, "push_forall mask")
                if (validation%is_failure()) then
                    write(*, '(A)') "ERROR [ast_factory_control]: " // validation%get_full_message()
                    forall_index = 0
                    return
                end if
            end if
        end if

        ! Validate body indices
        if (present(body_indices)) then
            do i = 1, size(body_indices)
                validation = validate_node_index(arena, body_indices(i), "push_forall body")
                if (validation%is_failure()) then
                    write(*, '(A)') "ERROR [ast_factory_control]: " // validation%get_full_message()
                    forall_index = 0
                    return
                end if
            end do
        end if

        ! For simple single-index FORALL, use first element of arrays
        forall_stmt%num_indices = 1
        allocate(character(len=len(index_var)) :: forall_stmt%index_names(1))
        forall_stmt%index_names(1) = index_var

        ! Set start and end expression indices
        allocate(forall_stmt%lower_bound_indices(1))
        allocate(forall_stmt%upper_bound_indices(1))
        allocate(forall_stmt%stride_indices(1))
        
        forall_stmt%lower_bound_indices(1) = start_index
        forall_stmt%upper_bound_indices(1) = end_index

        ! Set optional step expression index
        if (present(step_index)) then
            if (step_index > 0) then
                forall_stmt%stride_indices(1) = step_index
            else
                forall_stmt%stride_indices(1) = 0
            end if
        else
            forall_stmt%stride_indices(1) = 0
        end if

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

        call arena%push(forall_stmt, "forall", parent_index)
        forall_index = arena%size
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
    end function push_select_case

    ! Create select case node with default case and add to stack
    function push_select_case_with_default(arena, selector_index, case_indices, &
                                           default_index, &
                                        line, column, parent_index) result(select_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: selector_index
        integer, intent(in), optional :: case_indices(:)
        integer, intent(in) :: default_index
        integer, intent(in), optional :: line, column, parent_index
        integer :: select_index
        type(select_case_node) :: select_node

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
    end function push_case_block

    ! Create case range node and add to stack
    function push_case_range(arena, start_value, end_value, line, column, &
                            parent_index) result(range_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: start_value, end_value
        integer, intent(in), optional :: line, column, parent_index
        integer :: range_index
        type(case_range_node) :: range_node

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
    end function push_case_default

    ! Create WHERE construct node and add to stack
    function push_where(arena, mask_expr_index, where_body_indices, &
                       elsewhere_body_indices, &
                        line, column, parent_index) result(where_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: elsewhere_body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: where_index
        type(where_node) :: where_stmt
        integer :: i
        type(result_t) :: validation

        ! Validate arena is initialized
        validation = validate_arena(arena, "push_where")
        if (validation%is_failure()) then
            write(*, '(A)') "ERROR [ast_factory_control]: " // validation%get_full_message()
            where_index = 0
            return
        end if

        ! Validate mask expression index
        validation = validate_node_index(arena, mask_expr_index, "push_where mask")
        if (validation%is_failure()) then
            write(*, '(A)') "ERROR [ast_factory_control]: " // validation%get_full_message()
            where_index = 0
            return
        end if

        ! Validate where body indices
        if (present(where_body_indices)) then
            do i = 1, size(where_body_indices)
                validation = validate_node_index(arena, where_body_indices(i), "push_where body")
                if (validation%is_failure()) then
                    write(*, '(A)') "ERROR [ast_factory_control]: " // validation%get_full_message()
                    where_index = 0
                    return
                end if
            end do
        end if

        ! Validate elsewhere body indices
        if (present(elsewhere_body_indices)) then
            do i = 1, size(elsewhere_body_indices)
                validation = validate_node_index(arena, elsewhere_body_indices(i), "push_where elsewhere")
                if (validation%is_failure()) then
                    write(*, '(A)') "ERROR [ast_factory_control]: " // validation%get_full_message()
                    where_index = 0
                    return
                end if
            end do
        end if

        where_stmt = create_where(mask_expr_index=mask_expr_index, &
                                  where_body_indices=where_body_indices, &
                                  elsewhere_body_indices=elsewhere_body_indices, &
                                  line=line, column=column)

        call arena%push(where_stmt, "where_node", parent_index)
        where_index = arena%size
    end function push_where

    ! Create WHERE construct node and add to stack (simplified interface)
    function push_where_construct(arena, mask_expr_index, where_body_indices, &
                                  line, column, parent_index) result(where_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: where_index

        where_index = push_where(arena, mask_expr_index, where_body_indices, &
                                 line=line, column=column, parent_index=parent_index)
    end function push_where_construct

    ! Create WHERE construct with ELSEWHERE and add to stack
    function push_where_construct_with_elsewhere(arena, mask_expr_index, &
                                                 where_body_indices, &
                                   elsewhere_body_indices, line, column, parent_index) &
        result(where_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: elsewhere_body_indices(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: where_index

        where_index = push_where(arena, mask_expr_index, where_body_indices, &
                                 elsewhere_body_indices, line, column, parent_index)
    end function push_where_construct_with_elsewhere

end module ast_factory_control