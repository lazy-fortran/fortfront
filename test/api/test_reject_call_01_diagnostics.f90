program test_reject_call_01_diagnostics
    ! Rejection coverage for procedure-call signature mismatches (issue #2882).
    !
    ! Rule under test: the procedure designator of a CALL statement must name a
    ! subroutine.  A name that has a type in an accessible scoping unit -- a
    ! data object, or a function -- is not a subroutine, so the CALL is invalid.
    ! Represented by gfortran.dg/typed_subroutine_1.f90.
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== Reject invalid CALL targets (issue #2882) ==='

    call test_call_of_typed_variable_rejected(all_tests_passed)
    call test_call_of_multi_declared_variable_rejected(all_tests_passed)
    call test_call_of_contained_function_rejected(all_tests_passed)
    call test_call_of_contained_subroutine_accepted(all_tests_passed)
    call test_call_of_external_declared_name_accepted(all_tests_passed)
    call test_call_of_intrinsic_subroutine_accepted(all_tests_passed)
    call test_call_of_undeclared_name_accepted(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All CALL target rejection tests passed'
        stop 0
    else
        print *, 'Some CALL target rejection tests failed'
        stop 1
    end if

contains

    subroutine test_call_of_typed_variable_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of a typed local variable (rejected)...'
        source = 'integer :: s'//new_line('a')// &
            'call s()'//new_line('a')// &
            'end'
        call expect_frontend_error(source, 'has a type', passed)
    end subroutine test_call_of_typed_variable_rejected

    subroutine test_call_of_multi_declared_variable_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of a name from a multi declaration (rejected)...'
        source = 'integer :: a, s, b'//new_line('a')// &
            'call s()'//new_line('a')// &
            'end'
        call expect_frontend_error(source, 'not consistent with the CALL', passed)
    end subroutine test_call_of_multi_declared_variable_rejected

    subroutine test_call_of_contained_function_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of a contained function (rejected)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'call f()'//new_line('a')// &
            'contains'//new_line('a')// &
            'function f() result(v)'//new_line('a')// &
            'real :: v'//new_line('a')// &
            'v = 1'//new_line('a')// &
            'end function f'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, 'has a type', passed)
    end subroutine test_call_of_contained_function_rejected

    subroutine test_call_of_contained_subroutine_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of a contained subroutine (accepted)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'call f()'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine f()'//new_line('a')// &
            'print *, 1'//new_line('a')// &
            'end subroutine f'//new_line('a')// &
            'end program p'
        call expect_frontend_accepts(source, passed)
    end subroutine test_call_of_contained_subroutine_accepted

    subroutine test_call_of_external_declared_name_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of an EXTERNAL declared name (accepted)...'
        source = 'integer, external :: s'//new_line('a')// &
            'call s()'//new_line('a')// &
            'end'
        call expect_frontend_accepts(source, passed)
    end subroutine test_call_of_external_declared_name_accepted

    subroutine test_call_of_intrinsic_subroutine_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of an intrinsic subroutine (accepted)...'
        source = 'real :: random_number_result'//new_line('a')// &
            'call random_number(random_number_result)'//new_line('a')// &
            'end'
        call expect_frontend_accepts(source, passed)
    end subroutine test_call_of_intrinsic_subroutine_accepted

    subroutine test_call_of_undeclared_name_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing CALL of an undeclared external name (accepted)...'
        source = 'call s()'//new_line('a')// &
            'end'
        call expect_frontend_accepts(source, passed)
    end subroutine test_call_of_undeclared_name_accepted

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

    subroutine expect_frontend_accepts(source, passed)
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
    end subroutine expect_frontend_accepts

end program test_reject_call_01_diagnostics
