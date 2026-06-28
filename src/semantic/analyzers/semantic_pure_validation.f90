module semantic_pure_validation
    ! Validates PURE/ELEMENTAL procedure bodies per F2008 C1283.
    ! A PURE procedure must not perform external I/O (PRINT, WRITE to an
    ! external unit, READ) or execute an image-control / program-halting
    ! statement such as STOP or PAUSE. ELEMENTAL implies PURE, so the same
    ! body restrictions apply.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_io, only: print_statement_node, write_statement_node, &
        read_statement_node
    use ast_nodes_transfer, only: stop_node, pause_node
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_nodes_conditional, only: if_node
    use error_handling, only: error_collection_t, ERROR_SEMANTIC
    use string_utils_mod, only: int_to_string
    implicit none
    private

    public :: validate_pure_procedure, is_pure_prefix

contains

    ! Returns .true. when the prefix list marks the procedure as PURE,
    ! either explicitly (pure) or implicitly (elemental). An explicit
    ! impure keyword overrides elemental.
    function is_pure_prefix(prefix_keywords) result(is_pure)
        character(len=*), allocatable, intent(in) :: prefix_keywords(:)
        logical :: is_pure
        logical :: has_pure, has_elemental, has_impure
        integer :: i

        is_pure = .false.
        if (.not. allocated(prefix_keywords)) return

        has_pure = .false.
        has_elemental = .false.
        has_impure = .false.
        do i = 1, size(prefix_keywords)
            select case (trim(prefix_keywords(i)))
            case ('pure')
                has_pure = .true.
            case ('elemental')
                has_elemental = .true.
            case ('impure')
                has_impure = .true.
            end select
        end do

        if (has_impure) return
        is_pure = has_pure .or. has_elemental
    end function is_pure_prefix

    ! Validate a procedure body when its prefix marks it PURE/ELEMENTAL.
    ! Non-pure procedures are accepted unchanged.
    subroutine validate_pure_procedure(arena, body_indices, prefix_keywords, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), allocatable, intent(in) :: prefix_keywords(:)
        type(error_collection_t), intent(inout) :: errors

        if (.not. is_pure_prefix(prefix_keywords)) return
        if (.not. allocated(body_indices)) return

        call check_pure_body(arena, body_indices, errors)
    end subroutine validate_pure_procedure

    ! Recursively scan a list of body statements for prohibited statements.
    recursive subroutine check_pure_body(arena, body_indices, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        type(error_collection_t), intent(inout) :: errors
        integer :: i, stmt_index

        do i = 1, size(body_indices)
            stmt_index = body_indices(i)
            if (.not. arena%has_node_at(stmt_index)) cycle
            call check_pure_statement(arena, stmt_index, errors)
        end do
    end subroutine check_pure_body

    ! Inspect a single statement: report it if prohibited, recurse into the
    ! bodies of nested control-flow constructs otherwise.
    recursive subroutine check_pure_statement(arena, stmt_index, errors)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        type(error_collection_t), intent(inout) :: errors

        select type (stmt => arena%entries(stmt_index)%node)
            type is (print_statement_node)
            call report_impure_stmt(errors, 'PRINT', stmt%line, stmt%column)
            type is (write_statement_node)
            call report_impure_stmt(errors, 'WRITE', stmt%line, stmt%column)
            type is (read_statement_node)
            call report_impure_stmt(errors, 'READ', stmt%line, stmt%column)
            type is (stop_node)
            call report_impure_stmt(errors, 'STOP', stmt%line, stmt%column)
            type is (pause_node)
            call report_impure_stmt(errors, 'PAUSE', stmt%line, stmt%column)
            type is (if_node)
            call check_pure_if(arena, stmt, errors)
            type is (do_loop_node)
            if (allocated(stmt%body_indices)) &
                call check_pure_body(arena, stmt%body_indices, errors)
            type is (do_while_node)
            if (allocated(stmt%body_indices)) &
                call check_pure_body(arena, stmt%body_indices, errors)
        end select
    end subroutine check_pure_statement

    ! Recurse into all branches of an IF construct.
    recursive subroutine check_pure_if(arena, node, errors)
        type(ast_arena_t), intent(in) :: arena
        type(if_node), intent(in) :: node
        type(error_collection_t), intent(inout) :: errors
        integer :: i

        if (allocated(node%then_body_indices)) &
            call check_pure_body(arena, node%then_body_indices, errors)
        if (allocated(node%elseif_blocks)) then
            do i = 1, size(node%elseif_blocks)
                if (allocated(node%elseif_blocks(i)%body_indices)) &
                    call check_pure_body(arena, &
                    node%elseif_blocks(i)%body_indices, errors)
            end do
        end if
        if (allocated(node%else_body_indices)) &
            call check_pure_body(arena, node%else_body_indices, errors)
    end subroutine check_pure_if

    subroutine report_impure_stmt(errors, kind, line, column)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: kind
        integer, intent(in) :: line, column

        call errors%add_error( &
            message=kind//' statement is not allowed in a PURE procedure', &
            code=ERROR_SEMANTIC, &
            component='semantic_pure_validation', &
            context='line '//int_to_string(line)//', column '// &
            int_to_string(column), &
            suggestion='remove the I/O or control statement, or drop the '// &
            'PURE/ELEMENTAL prefix')
    end subroutine report_impure_stmt

end module semantic_pure_validation
