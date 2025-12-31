
    ! Create WHERE construct node and add to stack
    function push_where(arena, mask_expr_index, where_body_indices, &
                        elsewhere_body_indices, elsewhere_clauses, &
                        line, column, parent_index) result(where_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: elsewhere_body_indices(:)
        type(elsewhere_clause_t), intent(in), optional :: elsewhere_clauses(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: where_index
        type(where_node) :: where_stmt
        integer :: i, j
        type(result_t) :: validation

        where_stmt%uid = generate_uid()
        where_stmt%mask_is_simple = .false.
        where_stmt%can_vectorize = .false.

        ! Validate arena is initialized
        validation = validate_arena(arena, "push_where")
        if (validation%is_failure()) then
            write (error_unit, '(A)') &
                "ERROR [ast_factory_control]: " // &
                validation%get_full_message()
            where_index = 0
            return
        end if

        ! Validate mask expression index
        validation = validate_node_index(arena, mask_expr_index, "push_where mask")
        if (validation%is_failure()) then
            write (error_unit, '(A)') &
                "ERROR [ast_factory_control]: " // &
                validation%get_full_message()
            where_index = 0
            return
        end if

        ! Validate where body indices
        if (present(where_body_indices)) then
            do i = 1, size(where_body_indices)
                validation = validate_node_index( &
                    arena, where_body_indices(i), "push_where body")
                if (validation%is_failure()) then
                    write (error_unit, '(A)') &
                        "ERROR [ast_factory_control]: " // &
                        validation%get_full_message()
                    where_index = 0
                    return
                end if
            end do
        end if

        ! Validate elsewhere body indices
        if (present(elsewhere_body_indices)) then
            do i = 1, size(elsewhere_body_indices)
                validation = validate_node_index( &
                    arena, elsewhere_body_indices(i), "push_where elsewhere")
                if (validation%is_failure()) then
                    write (error_unit, '(A)') &
                        "ERROR [ast_factory_control]: " // &
                        validation%get_full_message()
                    where_index = 0
                    return
                end if
            end do
        end if
        if (present(elsewhere_clauses)) then
            do i = 1, size(elsewhere_clauses)
                if (elsewhere_clauses(i)%mask_index > 0) then
                    validation = validate_node_index( &
                        arena, elsewhere_clauses(i)%mask_index, &
                        "push_where elsewhere mask")
                    if (validation%is_failure()) then
                        write (error_unit, '(A)') &
                            "ERROR [ast_factory_control]: " // &
                            validation%get_full_message()
                        where_index = 0
                        return
                    end if
                end if
                if (allocated(elsewhere_clauses(i)%body_indices)) then
                    do j = 1, size(elsewhere_clauses(i)%body_indices)
                        validation = validate_node_index( &
                            arena, elsewhere_clauses(i)%body_indices(j), &
                            "push_where elsewhere body")
                        if (validation%is_failure()) then
                            write (error_unit, '(A)') &
                                "ERROR [ast_factory_control]: " // &
                                validation%get_full_message()
                            where_index = 0
                            return
                        end if
                    end do
                end if
            end do
        end if

        where_stmt%mask_expr_index = mask_expr_index
        if (present(where_body_indices)) then
            if (size(where_body_indices) > 0) then
                where_stmt%where_body_indices = where_body_indices
            end if
        end if
        if (present(elsewhere_clauses)) then
            if (size(elsewhere_clauses) > 0) then
                allocate (where_stmt%elsewhere_clauses(size(elsewhere_clauses)))
                do i = 1, size(elsewhere_clauses)
                    where_stmt%elsewhere_clauses(i)%mask_index = &
                        elsewhere_clauses(i)%mask_index
                    if (allocated(elsewhere_clauses(i)%body_indices)) then
                        allocate (where_stmt%elsewhere_clauses(i)%body_indices( &
                                  size(elsewhere_clauses(i)%body_indices)))
                        where_stmt%elsewhere_clauses(i)%body_indices = &
                            elsewhere_clauses(i)%body_indices
                    end if
                end do
            end if
        else if (present(elsewhere_body_indices)) then
            if (size(elsewhere_body_indices) > 0) then
                allocate (where_stmt%elsewhere_clauses(1))
                where_stmt%elsewhere_clauses(1)%mask_index = 0
                allocate (where_stmt%elsewhere_clauses(1)%body_indices( &
                          size(elsewhere_body_indices)))
                where_stmt%elsewhere_clauses(1)%body_indices = &
                    elsewhere_body_indices
            end if
        end if
        if (present(line)) where_stmt%line = line
        if (present(column)) where_stmt%column = column

        call arena%push(where_stmt, "where_node", parent_index)
        where_index = arena%size

        ! Link children to this parent for AST traversal
        if (mask_expr_index > 0) then
            call link_children_to_parent(arena, where_index, [mask_expr_index])
        end if
        if (present(where_body_indices)) then
            if (size(where_body_indices) > 0) then
                call link_children_to_parent(arena, where_index, where_body_indices)
            end if
        end if
        if (present(elsewhere_body_indices)) then
            if (size(elsewhere_body_indices) > 0) then
                call link_children_to_parent(arena, where_index, elsewhere_body_indices)
            end if
        end if
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
                                 line=line, column=column, &
                                 parent_index=parent_index)
    end function push_where_construct

    ! Create WHERE construct with ELSEWHERE and add to stack
    function push_where_construct_with_elsewhere(arena, mask_expr_index, &
                                                 where_body_indices, &
                                                 elsewhere_body_indices, &
                                                 elsewhere_clauses, line, column, &
                                                 parent_index) result(where_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: elsewhere_body_indices(:)
        type(elsewhere_clause_t), intent(in), optional :: elsewhere_clauses(:)
        integer, intent(in), optional :: line, column, parent_index
        integer :: where_index

        if (present(elsewhere_clauses)) then
            if (present(where_body_indices)) then
                where_index = push_where(arena, mask_expr_index, &
                                         where_body_indices=where_body_indices, &
                                         elsewhere_clauses=elsewhere_clauses, &
                                         line=line, column=column, &
                                         parent_index=parent_index)
            else
                where_index = push_where(arena, mask_expr_index, &
                                         elsewhere_clauses=elsewhere_clauses, &
                                         line=line, column=column, &
                                         parent_index=parent_index)
            end if
        else
            if (present(where_body_indices) .and. present(elsewhere_body_indices)) then
                where_index = push_where(arena, mask_expr_index, &
                                         where_body_indices=where_body_indices, &
                                         elsewhere_body_indices=elsewhere_body_indices, &
                                         line=line, column=column, &
                                         parent_index=parent_index)
            else if (present(where_body_indices)) then
                where_index = push_where(arena, mask_expr_index, &
                                         where_body_indices=where_body_indices, &
                                         line=line, column=column, &
                                         parent_index=parent_index)
            else if (present(elsewhere_body_indices)) then
                where_index = push_where(arena, mask_expr_index, &
                                         elsewhere_body_indices=elsewhere_body_indices, &
                                         line=line, column=column, &
                                         parent_index=parent_index)
            else
                where_index = push_where(arena, mask_expr_index, line=line, &
                                         column=column, parent_index=parent_index)
            end if
        end if
    end function push_where_construct_with_elsewhere
