program test_where_validation
    ! Tests that WHERE/ELSEWHERE body validation rejects invalid statements
    ! per F2018 Section 10.2.3.2
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_array, only: where_node, elsewhere_clause_t
    use ast_base, only: LITERAL_LOGICAL, LITERAL_INTEGER
    use ast_factory, only: push_where
    use ast_factory_core, only: push_assignment, push_identifier, push_literal
    use ast_factory_control, only: push_if
    use ast_factory_statements, only: push_stop
    use error_handling, only: error_collection_t, create_error_collection
    use semantic_where_validation, only: validate_where_construct
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== WHERE Validation Tests ==='
    print *

    call test_valid_where_body_with_assignments(all_tests_passed)
    call test_invalid_where_body_with_if(all_tests_passed)
    call test_invalid_where_body_with_stop(all_tests_passed)
    call test_valid_elsewhere_body(all_tests_passed)
    call test_invalid_elsewhere_body(all_tests_passed)

    print *
    if (all_tests_passed) then
        print *, 'All WHERE validation tests PASSED!'
        stop 0
    else
        print *, 'Some WHERE validation tests FAILED!'
        stop 1
    end if

contains

    subroutine test_valid_where_body_with_assignments(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer :: where_idx, mask_idx, assign_idx, target_idx, value_idx
        integer, allocatable :: body_indices(:)

        print *, 'Testing valid WHERE body with assignments...'

        arena = create_ast_arena()
        errors = create_error_collection()

        ! Create mask expression
        mask_idx = push_literal(arena, '.true.', LITERAL_LOGICAL, line=1, column=1)

        ! Create valid assignment statement
        target_idx = push_identifier(arena, 'a', line=2, column=1)
        value_idx = push_literal(arena, '1', LITERAL_INTEGER, line=2, column=5)
        assign_idx = push_assignment(arena, target_idx, value_idx, line=2, column=1)

        ! Create WHERE node with assignment body
        allocate (body_indices(1))
        body_indices(1) = assign_idx
        where_idx = push_where(arena, mask_idx, body_indices, line=1, column=1)

        ! Validate - should have no errors
        block
            type(where_node) :: where_nd
            select type (node => arena%entries(where_idx)%node)
            type is (where_node)
                where_nd = node
            end select
            call validate_where_construct(arena, where_nd, errors)
        end block

        if (errors%has_errors()) then
            print *, '  FAIL: Valid WHERE body incorrectly rejected'
            passed = .false.
        else
            print *, '  PASS: Valid WHERE body accepted'
        end if
    end subroutine test_valid_where_body_with_assignments

    subroutine test_invalid_where_body_with_if(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer :: where_idx, mask_idx, if_idx, cond_idx
        integer, allocatable :: body_indices(:), if_body(:)

        print *, 'Testing invalid WHERE body with IF construct...'

        arena = create_ast_arena()
        errors = create_error_collection()

        ! Create mask expression
        mask_idx = push_literal(arena, '.true.', LITERAL_LOGICAL, line=1, column=1)

        ! Create invalid IF construct
        cond_idx = push_literal(arena, '.true.', LITERAL_LOGICAL, line=2, column=1)
        allocate (if_body(0))
        if_idx = push_if(arena, cond_idx, if_body, line=2, column=1)

        ! Create WHERE node with IF body (invalid)
        allocate (body_indices(1))
        body_indices(1) = if_idx
        where_idx = push_where(arena, mask_idx, body_indices, line=1, column=1)

        ! Validate - should have error
        block
            type(where_node) :: where_nd
            select type (node => arena%entries(where_idx)%node)
            type is (where_node)
                where_nd = node
            end select
            call validate_where_construct(arena, where_nd, errors)
        end block

        if (errors%has_errors()) then
            print *, '  PASS: Invalid IF in WHERE body correctly rejected'
        else
            print *, '  FAIL: Invalid IF in WHERE body not detected'
            passed = .false.
        end if
    end subroutine test_invalid_where_body_with_if

    subroutine test_invalid_where_body_with_stop(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer :: where_idx, mask_idx, stop_idx
        integer, allocatable :: body_indices(:)

        print *, 'Testing invalid WHERE body with STOP statement...'

        arena = create_ast_arena()
        errors = create_error_collection()

        ! Create mask expression
        mask_idx = push_literal(arena, '.true.', LITERAL_LOGICAL, line=1, column=1)

        ! Create invalid STOP statement
        stop_idx = push_stop(arena, 0, line=2, column=1)

        ! Create WHERE node with STOP body (invalid)
        allocate (body_indices(1))
        body_indices(1) = stop_idx
        where_idx = push_where(arena, mask_idx, body_indices, line=1, column=1)

        ! Validate - should have error
        block
            type(where_node) :: where_nd
            select type (node => arena%entries(where_idx)%node)
            type is (where_node)
                where_nd = node
            end select
            call validate_where_construct(arena, where_nd, errors)
        end block

        if (errors%has_errors()) then
            print *, '  PASS: Invalid STOP in WHERE body correctly rejected'
        else
            print *, '  FAIL: Invalid STOP in WHERE body not detected'
            passed = .false.
        end if
    end subroutine test_invalid_where_body_with_stop

    subroutine test_valid_elsewhere_body(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        type(elsewhere_clause_t), allocatable :: elsewhere_clauses(:)
        integer :: where_idx, mask_idx, assign_idx, target_idx, value_idx
        integer, allocatable :: body_indices(:)

        print *, 'Testing valid ELSEWHERE body with assignments...'

        arena = create_ast_arena()
        errors = create_error_collection()

        ! Create mask expression
        mask_idx = push_literal(arena, '.true.', LITERAL_LOGICAL, line=1, column=1)

        ! Create valid assignment for elsewhere
        target_idx = push_identifier(arena, 'b', line=3, column=1)
        value_idx = push_literal(arena, '2', LITERAL_INTEGER, line=3, column=5)
        assign_idx = push_assignment(arena, target_idx, value_idx, line=3, column=1)

        ! Create ELSEWHERE clause with valid assignment
        allocate (elsewhere_clauses(1))
        elsewhere_clauses(1)%mask_index = 0 ! Unmasked ELSEWHERE
        allocate (elsewhere_clauses(1)%body_indices(1))
        elsewhere_clauses(1)%body_indices(1) = assign_idx

        ! Create WHERE node with empty body and valid ELSEWHERE
        allocate (body_indices(0))
        where_idx = push_where(arena, mask_idx, body_indices, &
                               elsewhere_clauses=elsewhere_clauses, &
                               line=1, column=1)

        ! Validate - should have no errors
        block
            type(where_node) :: where_nd
            select type (node => arena%entries(where_idx)%node)
            type is (where_node)
                where_nd = node
            end select
            call validate_where_construct(arena, where_nd, errors)
        end block

        if (errors%has_errors()) then
            print *, '  FAIL: Valid ELSEWHERE body incorrectly rejected'
            passed = .false.
        else
            print *, '  PASS: Valid ELSEWHERE body accepted'
        end if
    end subroutine test_valid_elsewhere_body

    subroutine test_invalid_elsewhere_body(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        type(elsewhere_clause_t), allocatable :: elsewhere_clauses(:)
        integer :: where_idx, mask_idx, stop_idx
        integer, allocatable :: body_indices(:)

        print *, 'Testing invalid ELSEWHERE body with STOP...'

        arena = create_ast_arena()
        errors = create_error_collection()

        ! Create mask expression
        mask_idx = push_literal(arena, '.true.', LITERAL_LOGICAL, line=1, column=1)

        ! Create invalid STOP statement for elsewhere
        stop_idx = push_stop(arena, 0, line=3, column=1)

        ! Create ELSEWHERE clause with invalid STOP
        allocate (elsewhere_clauses(1))
        elsewhere_clauses(1)%mask_index = 0
        allocate (elsewhere_clauses(1)%body_indices(1))
        elsewhere_clauses(1)%body_indices(1) = stop_idx

        ! Create WHERE node with empty body and invalid ELSEWHERE
        allocate (body_indices(0))
        where_idx = push_where(arena, mask_idx, body_indices, &
                               elsewhere_clauses=elsewhere_clauses, &
                               line=1, column=1)

        ! Validate - should have error
        block
            type(where_node) :: where_nd
            select type (node => arena%entries(where_idx)%node)
            type is (where_node)
                where_nd = node
            end select
            call validate_where_construct(arena, where_nd, errors)
        end block

        if (errors%has_errors()) then
            print *, '  PASS: Invalid STOP in ELSEWHERE correctly rejected'
        else
            print *, '  FAIL: Invalid STOP in ELSEWHERE not detected'
            passed = .false.
        end if
    end subroutine test_invalid_elsewhere_body

end program test_where_validation
