
    ! Create WHERE construct node and add to stack
    function push_where(arena, mask_expr_index, where_body_indices, &
                        elsewhere_body_indices, elsewhere_clauses, &
                        line, column, parent_index, is_single_line) &
                        result(where_index)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: elsewhere_body_indices(:)
        type(elsewhere_clause_t), intent(in), optional :: elsewhere_clauses(:)
        integer, intent(in), optional :: line, column, parent_index
        logical, intent(in), optional :: is_single_line
        integer :: where_index
        type(where_node) :: where_stmt
        integer :: i

        where_stmt%uid = generate_uid()
        where_stmt%mask_is_simple = .false.
        where_stmt%can_vectorize = .false.
        if (present(is_single_line)) where_stmt%is_single_line = is_single_line

        if (.not. where_inputs_are_valid(arena, mask_expr_index, &
                where_body_indices, elsewhere_body_indices, &
                elsewhere_clauses)) then
            where_index = 0
            return
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
        call link_where_children(arena, where_index, mask_expr_index, &
            where_body_indices, elsewhere_body_indices, elsewhere_clauses)
    end function push_where

    logical function where_inputs_are_valid(arena, mask_expr_index, &
            where_body_indices, elsewhere_body_indices, elsewhere_clauses)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: elsewhere_body_indices(:)
        type(elsewhere_clause_t), intent(in), optional :: elsewhere_clauses(:)
        type(result_t) :: validation
        integer :: i

        where_inputs_are_valid = .false.
        validation = validate_arena(arena, 'push_where')
        if (.not. report_valid(validation)) return
        validation = validate_node_index(arena, mask_expr_index, 'push_where mask')
        if (.not. report_valid(validation)) return
        if (present(where_body_indices)) then
            if (.not. index_list_is_valid(arena, where_body_indices, &
                    'push_where body')) return
        end if
        if (present(elsewhere_body_indices)) then
            if (.not. index_list_is_valid(arena, elsewhere_body_indices, &
                    'push_where elsewhere')) return
        end if
        if (present(elsewhere_clauses)) then
            do i = 1, size(elsewhere_clauses)
                if (.not. elsewhere_clause_is_valid(arena, &
                        elsewhere_clauses(i))) return
            end do
        end if
        where_inputs_are_valid = .true.
    end function where_inputs_are_valid

    logical function elsewhere_clause_is_valid(arena, clause)
        type(ast_arena_t), intent(in) :: arena
        type(elsewhere_clause_t), intent(in) :: clause
        type(result_t) :: validation

        elsewhere_clause_is_valid = .false.
        if (clause%mask_index > 0) then
            validation = validate_node_index(arena, clause%mask_index, &
                'push_where elsewhere mask')
            if (.not. report_valid(validation)) return
        end if
        if (allocated(clause%body_indices)) then
            if (.not. index_list_is_valid(arena, clause%body_indices, &
                    'push_where elsewhere body')) return
        end if
        elsewhere_clause_is_valid = .true.
    end function elsewhere_clause_is_valid

    logical function index_list_is_valid(arena, indices, context)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: indices(:)
        character(len=*), intent(in) :: context
        type(result_t) :: validation
        integer :: i

        index_list_is_valid = .false.
        do i = 1, size(indices)
            validation = validate_node_index(arena, indices(i), context)
            if (.not. report_valid(validation)) return
        end do
        index_list_is_valid = .true.
    end function index_list_is_valid

    logical function report_valid(validation)
        type(result_t), intent(in) :: validation

        report_valid = .not. validation%is_failure()
        if (report_valid) return
        write (error_unit, '(A)') 'ERROR [ast_factory_control]: '// &
            validation%get_full_message()
    end function report_valid

    subroutine link_where_children(arena, where_index, mask_expr_index, &
            where_body_indices, elsewhere_body_indices, elsewhere_clauses)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: where_index, mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: elsewhere_body_indices(:)
        type(elsewhere_clause_t), intent(in), optional :: elsewhere_clauses(:)
        integer :: i

        call link_children_to_parent(arena, where_index, [mask_expr_index])
        if (present(where_body_indices)) then
            if (size(where_body_indices) > 0) call link_children_to_parent( &
                arena, where_index, where_body_indices)
        end if
        if (present(elsewhere_body_indices)) then
            if (size(elsewhere_body_indices) > 0) call link_children_to_parent( &
                arena, where_index, elsewhere_body_indices)
        end if
        if (.not. present(elsewhere_clauses)) return
        do i = 1, size(elsewhere_clauses)
            if (elsewhere_clauses(i)%mask_index > 0) call link_children_to_parent( &
                arena, where_index, [elsewhere_clauses(i)%mask_index])
            if (allocated(elsewhere_clauses(i)%body_indices)) then
                if (size(elsewhere_clauses(i)%body_indices) > 0) &
                    call link_children_to_parent(arena, where_index, &
                    elsewhere_clauses(i)%body_indices)
            end if
        end do
    end subroutine link_where_children

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
