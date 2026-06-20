program test_pure_validation
    ! Tests that PURE/ELEMENTAL procedure body validation rejects prohibited
    ! statements (external I/O, STOP, PAUSE) per F2008 C1283.
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_base, only: LITERAL_LOGICAL, LITERAL_INTEGER
    use ast_factory_core, only: push_assignment, push_identifier, push_literal
    use ast_factory_io, only: push_print_statement
    use ast_factory_statements, only: push_stop
    use ast_factory_control, only: push_if
    use error_handling, only: error_collection_t, create_error_collection
    use semantic_pure_validation, only: validate_pure_procedure, is_pure_prefix
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== PURE/ELEMENTAL Validation Tests ==='
    print *

    call test_pure_with_assignment_only(all_tests_passed)
    call test_pure_with_print_rejected(all_tests_passed)
    call test_pure_with_stop_rejected(all_tests_passed)
    call test_elemental_with_print_rejected(all_tests_passed)
    call test_impure_overrides_elemental(all_tests_passed)
    call test_non_pure_print_accepted(all_tests_passed)
    call test_pure_nested_if_print_rejected(all_tests_passed)
    call test_is_pure_prefix_classification(all_tests_passed)

    print *
    if (all_tests_passed) then
        print *, 'All PURE validation tests PASSED!'
        stop 0
    else
        print *, 'Some PURE validation tests FAILED!'
        stop 1
    end if

contains

    function make_assignment_body(arena) result(body)
        type(ast_arena_t), intent(inout) :: arena
        integer, allocatable :: body(:)
        integer :: target_idx, value_idx, assign_idx

        target_idx = push_identifier(arena, 'a', line=2, column=1)
        value_idx = push_literal(arena, '1', LITERAL_INTEGER, line=2, column=5)
        assign_idx = push_assignment(arena, target_idx, value_idx, line=2, column=1)
        allocate (body(1))
        body(1) = assign_idx
    end function make_assignment_body

    subroutine test_pure_with_assignment_only(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer, allocatable :: body(:)
        character(len=16), allocatable :: prefix(:)

        print *, 'Testing PURE procedure with only an assignment...'
        arena = create_ast_arena()
        errors = create_error_collection()
        body = make_assignment_body(arena)
        prefix = [character(len=16) :: 'pure']

        call validate_pure_procedure(arena, body, prefix, errors)

        if (errors%has_errors()) then
            print *, '  FAIL: valid PURE body incorrectly rejected'
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine test_pure_with_assignment_only

    subroutine test_pure_with_print_rejected(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer, allocatable :: body(:)
        integer :: print_idx
        character(len=16), allocatable :: prefix(:)

        print *, 'Testing PURE procedure with PRINT (rejected)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        print_idx = push_print_statement(arena, '*', line=2, column=1)
        allocate (body(1))
        body(1) = print_idx
        prefix = [character(len=16) :: 'pure']

        call validate_pure_procedure(arena, body, prefix, errors)

        if (errors%has_errors()) then
            print *, '  PASS'
        else
            print *, '  FAIL: PRINT in PURE body was not rejected'
            passed = .false.
        end if
    end subroutine test_pure_with_print_rejected

    subroutine test_pure_with_stop_rejected(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer, allocatable :: body(:)
        integer :: stop_idx
        character(len=16), allocatable :: prefix(:)

        print *, 'Testing PURE procedure with STOP (rejected)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        stop_idx = push_stop(arena, line=2, column=1)
        allocate (body(1))
        body(1) = stop_idx
        prefix = [character(len=16) :: 'pure']

        call validate_pure_procedure(arena, body, prefix, errors)

        if (errors%has_errors()) then
            print *, '  PASS'
        else
            print *, '  FAIL: STOP in PURE body was not rejected'
            passed = .false.
        end if
    end subroutine test_pure_with_stop_rejected

    subroutine test_elemental_with_print_rejected(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer, allocatable :: body(:)
        integer :: print_idx
        character(len=16), allocatable :: prefix(:)

        print *, 'Testing ELEMENTAL procedure with PRINT (rejected)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        print_idx = push_print_statement(arena, '*', line=2, column=1)
        allocate (body(1))
        body(1) = print_idx
        prefix = [character(len=16) :: 'elemental']

        call validate_pure_procedure(arena, body, prefix, errors)

        if (errors%has_errors()) then
            print *, '  PASS'
        else
            print *, '  FAIL: PRINT in ELEMENTAL body was not rejected'
            passed = .false.
        end if
    end subroutine test_elemental_with_print_rejected

    subroutine test_impure_overrides_elemental(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer, allocatable :: body(:)
        integer :: print_idx
        character(len=16), allocatable :: prefix(:)

        print *, 'Testing IMPURE ELEMENTAL with PRINT (accepted)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        print_idx = push_print_statement(arena, '*', line=2, column=1)
        allocate (body(1))
        body(1) = print_idx
        prefix = [character(len=16) :: 'impure', 'elemental']

        call validate_pure_procedure(arena, body, prefix, errors)

        if (errors%has_errors()) then
            print *, '  FAIL: IMPURE ELEMENTAL incorrectly rejected'
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine test_impure_overrides_elemental

    subroutine test_non_pure_print_accepted(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer, allocatable :: body(:)
        integer :: print_idx
        character(len=16), allocatable :: prefix(:)

        print *, 'Testing non-PURE procedure with PRINT (accepted)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        print_idx = push_print_statement(arena, '*', line=2, column=1)
        allocate (body(1))
        body(1) = print_idx
        allocate (prefix(0))

        call validate_pure_procedure(arena, body, prefix, errors)

        if (errors%has_errors()) then
            print *, '  FAIL: non-PURE body incorrectly rejected'
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine test_non_pure_print_accepted

    subroutine test_pure_nested_if_print_rejected(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer, allocatable :: body(:), then_body(:)
        integer :: cond_idx, print_idx, if_idx
        character(len=16), allocatable :: prefix(:)

        print *, 'Testing PURE with PRINT nested in IF (rejected)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        cond_idx = push_literal(arena, '.true.', LITERAL_LOGICAL, line=2, column=4)
        print_idx = push_print_statement(arena, '*', line=3, column=5)
        allocate (then_body(1))
        then_body(1) = print_idx
        if_idx = push_if(arena, cond_idx, then_body_indices=then_body, &
                         line=2, column=1)
        allocate (body(1))
        body(1) = if_idx
        prefix = [character(len=16) :: 'pure']

        call validate_pure_procedure(arena, body, prefix, errors)

        if (errors%has_errors()) then
            print *, '  PASS'
        else
            print *, '  FAIL: PRINT nested in IF was not rejected'
            passed = .false.
        end if
    end subroutine test_pure_nested_if_print_rejected

    subroutine test_is_pure_prefix_classification(passed)
        logical, intent(inout) :: passed
        character(len=16), allocatable :: prefix(:)
        logical :: ok

        print *, 'Testing is_pure_prefix classification...'
        ok = .true.

        prefix = [character(len=16) :: 'pure']
        if (.not. is_pure_prefix(prefix)) ok = .false.
        prefix = [character(len=16) :: 'elemental']
        if (.not. is_pure_prefix(prefix)) ok = .false.
        prefix = [character(len=16) :: 'recursive']
        if (is_pure_prefix(prefix)) ok = .false.
        prefix = [character(len=16) :: 'impure', 'elemental']
        if (is_pure_prefix(prefix)) ok = .false.

        if (ok) then
            print *, '  PASS'
        else
            print *, '  FAIL: is_pure_prefix misclassified a prefix set'
            passed = .false.
        end if
    end subroutine test_is_pure_prefix_classification

end program test_pure_validation
