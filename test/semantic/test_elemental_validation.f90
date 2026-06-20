program test_elemental_validation
    ! Tests that ELEMENTAL procedure dummy-argument validation rejects array
    ! dummies and accepts scalar dummies per F2008 C1290.
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_base, only: LITERAL_INTEGER
    use ast_factory_core, only: push_identifier, push_literal
    use ast_factory_declarations, only: push_declaration
    use error_handling, only: error_collection_t, create_error_collection
    use semantic_elemental_validation, only: validate_elemental_procedure, &
                                             is_elemental_prefix
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== ELEMENTAL Validation Tests ==='
    print *

    call test_elemental_scalar_dummy_accepted(all_tests_passed)
    call test_elemental_array_dummy_rejected(all_tests_passed)
    call test_elemental_inline_array_dummy_rejected(all_tests_passed)
    call test_non_elemental_array_dummy_accepted(all_tests_passed)
    call test_is_elemental_prefix_classification(all_tests_passed)

    print *
    if (all_tests_passed) then
        print *, 'All ELEMENTAL validation tests PASSED!'
        stop 0
    else
        print *, 'Some ELEMENTAL validation tests FAILED!'
        stop 1
    end if

contains

    subroutine test_elemental_scalar_dummy_accepted(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer, allocatable :: params(:), body(:)
        integer :: decl_idx
        character(len=16), allocatable :: prefix(:)

        print *, 'Testing ELEMENTAL with scalar dummy (accepted)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        allocate (params(1))
        params(1) = push_identifier(arena, 'x', line=1, column=1)
        decl_idx = push_declaration(arena, 'real', ['x'], line=2, column=1)
        allocate (body(1))
        body(1) = decl_idx
        prefix = [character(len=16) :: 'elemental']

        call validate_elemental_procedure(arena, params, body, prefix, errors)

        if (errors%has_errors()) then
            print *, '  FAIL: scalar dummy incorrectly rejected'
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine test_elemental_scalar_dummy_accepted

    subroutine test_elemental_array_dummy_rejected(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer, allocatable :: params(:), body(:), dims(:)
        integer :: decl_idx
        character(len=16), allocatable :: prefix(:)

        print *, 'Testing ELEMENTAL with array dummy in body (rejected)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        allocate (params(1))
        params(1) = push_identifier(arena, 'x', line=1, column=1)
        allocate (dims(1))
        dims(1) = push_literal(arena, '3', LITERAL_INTEGER, line=2, column=10)
        decl_idx = push_declaration(arena, 'real', ['x'], dimension_indices=dims, &
                                    line=2, column=1)
        allocate (body(1))
        body(1) = decl_idx
        prefix = [character(len=16) :: 'elemental']

        call validate_elemental_procedure(arena, params, body, prefix, errors)

        if (errors%has_errors()) then
            print *, '  PASS'
        else
            print *, '  FAIL: array dummy was not rejected'
            passed = .false.
        end if
    end subroutine test_elemental_array_dummy_rejected

    subroutine test_elemental_inline_array_dummy_rejected(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer, allocatable :: params(:), body(:), dims(:)
        character(len=16), allocatable :: prefix(:)

        print *, 'Testing ELEMENTAL with inline array param (rejected)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        allocate (dims(1))
        dims(1) = push_literal(arena, '3', LITERAL_INTEGER, line=1, column=10)
        allocate (params(1))
        params(1) = push_declaration(arena, 'real', ['x'], dimension_indices=dims, &
                                     line=1, column=1)
        allocate (body(0))
        prefix = [character(len=16) :: 'elemental']

        call validate_elemental_procedure(arena, params, body, prefix, errors)

        if (errors%has_errors()) then
            print *, '  PASS'
        else
            print *, '  FAIL: inline array param was not rejected'
            passed = .false.
        end if
    end subroutine test_elemental_inline_array_dummy_rejected

    subroutine test_non_elemental_array_dummy_accepted(passed)
        logical, intent(inout) :: passed
        type(ast_arena_t) :: arena
        type(error_collection_t) :: errors
        integer, allocatable :: params(:), body(:), dims(:)
        integer :: decl_idx
        character(len=16), allocatable :: prefix(:)

        print *, 'Testing non-ELEMENTAL with array dummy (accepted)...'
        arena = create_ast_arena()
        errors = create_error_collection()
        allocate (params(1))
        params(1) = push_identifier(arena, 'x', line=1, column=1)
        allocate (dims(1))
        dims(1) = push_literal(arena, '3', LITERAL_INTEGER, line=2, column=10)
        decl_idx = push_declaration(arena, 'real', ['x'], dimension_indices=dims, &
                                    line=2, column=1)
        allocate (body(1))
        body(1) = decl_idx
        prefix = [character(len=16) :: 'pure']

        call validate_elemental_procedure(arena, params, body, prefix, errors)

        if (errors%has_errors()) then
            print *, '  FAIL: non-ELEMENTAL array dummy incorrectly rejected'
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine test_non_elemental_array_dummy_accepted

    subroutine test_is_elemental_prefix_classification(passed)
        logical, intent(inout) :: passed
        character(len=16), allocatable :: prefix(:)
        logical :: ok

        print *, 'Testing is_elemental_prefix classification...'
        ok = .true.

        prefix = [character(len=16) :: 'elemental']
        if (.not. is_elemental_prefix(prefix)) ok = .false.
        prefix = [character(len=16) :: 'impure', 'elemental']
        if (.not. is_elemental_prefix(prefix)) ok = .false.
        prefix = [character(len=16) :: 'pure']
        if (is_elemental_prefix(prefix)) ok = .false.
        prefix = [character(len=16) :: 'recursive']
        if (is_elemental_prefix(prefix)) ok = .false.

        if (ok) then
            print *, '  PASS'
        else
            print *, '  FAIL: is_elemental_prefix misclassified a prefix set'
            passed = .false.
        end if
    end subroutine test_is_elemental_prefix_classification

end program test_elemental_validation
