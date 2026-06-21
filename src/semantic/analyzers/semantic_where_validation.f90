module semantic_where_validation
    ! Validates WHERE/ELSEWHERE construct body statements per F2018 Section 10.2.3.2
    ! Only allowed: array assignments, nested WHERE statements, nested WHERE constructs
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node
    use ast_nodes_array, only: where_node, where_stmt_node
    use error_handling, only: error_collection_t, result_t, create_error_result, &
                              ERROR_SEMANTIC
    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: validate_where_body, validate_where_construct

contains

    ! Validate an entire WHERE construct (main body + elsewhere clauses)
    subroutine validate_where_construct(arena, node, errors)
        type(ast_arena_t), intent(in) :: arena
        type(where_node), intent(in) :: node
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        ! Validate main WHERE body
        if (allocated(node%where_body_indices)) then
            call validate_where_body(arena, node%where_body_indices, errors, &
                                     node%line, node%column)
        end if

        ! Validate all ELSEWHERE clauses
        if (allocated(node%elsewhere_clauses)) then
            do i = 1, size(node%elsewhere_clauses)
                if (allocated(node%elsewhere_clauses(i)%body_indices)) then
                    call validate_where_body(arena, &
                                             node%elsewhere_clauses(i)%body_indices, &
                                             errors, node%line, node%column)
                end if
            end do
        end if
    end subroutine validate_where_construct

    ! Validate a list of body statements for WHERE/ELSEWHERE
    subroutine validate_where_body(arena, body_indices, errors, where_line, where_col)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: where_line, where_col
        integer :: i, stmt_index
        logical :: is_valid

        do i = 1, size(body_indices)
            stmt_index = body_indices(i)
            if (.not. arena%has_node_at(stmt_index)) cycle

            is_valid = is_valid_where_body_stmt(arena, stmt_index)

            if (.not. is_valid) then
                call report_invalid_where_stmt(arena, stmt_index, errors, &
                                               where_line, where_col)
            end if
        end do
    end subroutine validate_where_body

    ! Check if a statement is valid inside WHERE/ELSEWHERE body
    ! Per F2018 10.2.3.2: Only array assignments, WHERE stmts, WHERE constructs
    function is_valid_where_body_stmt(arena, stmt_index) result(is_valid)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        logical :: is_valid

        is_valid = .false.

        if (stmt_index <= 0 .or. stmt_index > arena%size) then
            is_valid = .true. ! Skip invalid indices
            return
        end if

        if (.not. allocated(arena%entries(stmt_index)%node)) then
            is_valid = .true. ! Skip unallocated nodes
            return
        end if

        select type (node => arena%entries(stmt_index)%node)
        type is (assignment_node)
            ! Array assignments are allowed
            is_valid = .true.
        type is (where_node)
            ! Nested WHERE constructs are allowed
            is_valid = .true.
        type is (where_stmt_node)
            ! Nested WHERE statements are allowed
            is_valid = .true.
        class default
            ! All other statement types are invalid in WHERE body
            is_valid = .false.
        end select
    end function is_valid_where_body_stmt

    ! Report an error for invalid statement inside WHERE
    subroutine report_invalid_where_stmt(arena, stmt_index, errors, where_line, &
                                         where_col)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: where_line, where_col
        type(result_t) :: error_result
        character(len=:), allocatable :: stmt_type
        character(len=:), allocatable :: msg
        integer :: stmt_line, stmt_col

        ! Get statement type name for error message
        stmt_type = get_stmt_type_name(arena, stmt_index)

        ! Get statement location
        stmt_line = arena%entries(stmt_index)%node%line
        stmt_col = arena%entries(stmt_index)%node%column

        msg = "Invalid statement in WHERE construct: "//stmt_type// &
              " (line "//trim(int_to_string(stmt_line))//")"

        error_result = create_error_result( &
                       msg, &
                       ERROR_SEMANTIC, &
                       component="semantic_where_validation", &
                       context="WHERE construct at line "// &
                       trim(int_to_string(where_line)), &
                       suggestion="WHERE/ELSEWHERE may only contain array "// &
                       "assignments, nested WHERE statements, or "// &
                       "nested WHERE constructs (F2018 10.2.3.2)" &
                       )

        call errors%add_result(error_result)
    end subroutine report_invalid_where_stmt

    ! Get human-readable statement type name
    function get_stmt_type_name(arena, stmt_index) result(name)
        use ast_nodes_control, only: if_node, do_loop_node, do_while_node, &
                                     stop_node, return_node, cycle_node, exit_node, &
                                     forall_node, select_case_node, pause_node
        use ast_nodes_io, only: print_statement_node, write_statement_node, &
                                read_statement_node
        use ast_nodes_misc, only: allocate_statement_node, deallocate_statement_node
        use ast_nodes_procedure, only: subroutine_call_node
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        character(len=:), allocatable :: name

        name = "unknown statement"

        if (.not. arena%has_node_at(stmt_index)) return

        select type (node => arena%entries(stmt_index)%node)
        type is (if_node)
            name = "IF construct"
        type is (do_loop_node)
            name = "DO loop"
        type is (do_while_node)
            name = "DO WHILE loop"
        type is (stop_node)
            name = "STOP statement"
        type is (return_node)
            name = "RETURN statement"
        type is (cycle_node)
            name = "CYCLE statement"
        type is (exit_node)
            name = "EXIT statement"
        type is (forall_node)
            name = "FORALL construct"
        type is (select_case_node)
            name = "SELECT CASE construct"
        type is (pause_node)
            name = "PAUSE statement"
        type is (print_statement_node)
            name = "PRINT statement"
        type is (write_statement_node)
            name = "WRITE statement"
        type is (read_statement_node)
            name = "READ statement"
        type is (allocate_statement_node)
            name = "ALLOCATE statement"
        type is (deallocate_statement_node)
            name = "DEALLOCATE statement"
        type is (subroutine_call_node)
            name = "CALL statement"
        class default
            name = "non-assignment statement"
        end select
    end function get_stmt_type_name

end module semantic_where_validation
