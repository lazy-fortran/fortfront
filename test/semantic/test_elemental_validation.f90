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
    call test_elemental_dummy_without_intent_rejected(all_tests_passed)
    call test_elemental_allocatable_dummy_rejected(all_tests_passed)
    call test_elemental_pointer_dummy_rejected(all_tests_passed)
    call test_elemental_dummy_procedure_rejected(all_tests_passed)
    call test_elemental_bind_c_rejected(all_tests_passed)
    call test_elemental_array_result_rejected(all_tests_passed)
    call test_elemental_allocatable_result_rejected(all_tests_passed)
    call test_elemental_pointer_result_rejected(all_tests_passed)
    call test_elemental_alternate_return_rejected(all_tests_passed)
    call test_elemental_value_dummy_accepted(all_tests_passed)
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
        decl_idx = push_declaration(arena, 'real', ['x'], intent_value='in', &
            line=2, column=1)
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

    subroutine test_elemental_dummy_without_intent_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ELEMENTAL dummy without intent/value (rejected)...'
        source = 'elemental subroutine s(x)'//new_line('a')// &
            'integer :: x'//new_line('a')// &
            'end subroutine s'
        call expect_frontend_error(source, 'INTENT', passed)
    end subroutine test_elemental_dummy_without_intent_rejected

    subroutine test_elemental_allocatable_dummy_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ELEMENTAL allocatable dummy (rejected)...'
        source = 'elemental subroutine s(x)'//new_line('a')// &
            'integer, allocatable, intent(in) :: x'//new_line('a')// &
            'end subroutine s'
        call expect_frontend_error(source, 'ALLOCATABLE dummy', passed)
    end subroutine test_elemental_allocatable_dummy_rejected

    subroutine test_elemental_pointer_dummy_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ELEMENTAL pointer dummy (rejected)...'
        source = 'elemental subroutine s(x)'//new_line('a')// &
            'integer, pointer, intent(in) :: x'//new_line('a')// &
            'end subroutine s'
        call expect_frontend_error(source, 'POINTER dummy', passed)
    end subroutine test_elemental_pointer_dummy_rejected

    subroutine test_elemental_dummy_procedure_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ELEMENTAL dummy procedure (rejected)...'
        source = 'elemental subroutine s(f)'//new_line('a')// &
            'interface'//new_line('a')// &
            'pure subroutine f()'//new_line('a')// &
            'end subroutine f'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end subroutine s'
        call expect_frontend_error(source, 'procedure dummy', passed)
    end subroutine test_elemental_dummy_procedure_rejected

    subroutine test_elemental_bind_c_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ELEMENTAL BIND(C) conflict (rejected)...'
        source = 'elemental subroutine s() bind(c)'//new_line('a')// &
            'end subroutine s'
        call expect_frontend_error(source, 'BIND(C)', passed)
    end subroutine test_elemental_bind_c_rejected

    subroutine test_elemental_array_result_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ELEMENTAL array result (rejected)...'
        source = 'elemental function f()'//new_line('a')// &
            'integer :: f(2)'//new_line('a')// &
            'end function f'
        call expect_frontend_error(source, 'array result', passed)
    end subroutine test_elemental_array_result_rejected

    subroutine test_elemental_allocatable_result_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ELEMENTAL allocatable result (rejected)...'
        source = 'elemental function f()'//new_line('a')// &
            'integer, allocatable :: f'//new_line('a')// &
            'end function f'
        call expect_frontend_error(source, 'ALLOCATABLE result', passed)
    end subroutine test_elemental_allocatable_result_rejected

    subroutine test_elemental_pointer_result_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ELEMENTAL pointer result (rejected)...'
        source = 'elemental function f()'//new_line('a')// &
            'integer, pointer :: f'//new_line('a')// &
            'end function f'
        call expect_frontend_error(source, 'pointer result', passed)
    end subroutine test_elemental_pointer_result_rejected

    subroutine test_elemental_alternate_return_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ELEMENTAL alternate return (rejected)...'
        source = 'elemental subroutine s(*)'//new_line('a')// &
            'return 1'//new_line('a')// &
            'end subroutine s'
        call expect_frontend_error(source, 'alternate return', passed)
    end subroutine test_elemental_alternate_return_rejected

    subroutine test_elemental_value_dummy_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ELEMENTAL VALUE dummy (accepted)...'
        source = 'elemental subroutine s(x)'//new_line('a')// &
            'integer, value :: x'//new_line('a')// &
            'end subroutine s'
        call expect_frontend_success(source, passed)
    end subroutine test_elemental_value_dummy_accepted

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

    subroutine expect_frontend_error(source, expected, passed)
        use frontend_compiler_api, only: compiler_frontend_options_t, &
            compiler_frontend_result_t, compile_frontend_from_string
        use semantic_input_mode, only: INPUT_MODE_STANDARD
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: expected
        logical, intent(inout) :: passed
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)

        if (result%success()) then
            print *, '  FAIL: invalid source was accepted'
            passed = .false.
            return
        end if
        if (index(result%diagnostic_text, expected) == 0) then
            print *, '  FAIL: diagnostic missing expected text: ', expected
            print *, trim(result%diagnostic_text)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_error

    subroutine expect_frontend_success(source, passed)
        use frontend_compiler_api, only: compiler_frontend_options_t, &
            compiler_frontend_result_t, compile_frontend_from_string
        use semantic_input_mode, only: INPUT_MODE_STANDARD
        character(len=*), intent(in) :: source
        logical, intent(inout) :: passed
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)

        if (.not. result%success()) then
            print *, '  FAIL: valid source was rejected'
            print *, trim(result%diagnostic_text)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_success

end program test_elemental_validation
