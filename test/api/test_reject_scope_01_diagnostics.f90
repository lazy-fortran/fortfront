program test_reject_scope_01_diagnostics
    ! Issue #2892: unresolved procedure references under IMPLICIT NONE
    ! (EXTERNAL) must be rejected (Fortran 2018 8.7 - with an EXTERNAL
    ! implicit none specifier a referenced procedure shall have an explicit
    ! interface or the EXTERNAL attribute). Each rejected form is paired with
    ! a corrected neighbour that must stay accepted.
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== reject-scope-01: unresolved procedure references ==='

    call test_undeclared_call_rejected(all_tests_passed)
    call test_interface_declared_call_accepted(all_tests_passed)
    call test_external_declared_call_accepted(all_tests_passed)
    call test_intrinsic_subroutine_accepted(all_tests_passed)
    call test_contained_procedure_accepted(all_tests_passed)
    call test_plain_implicit_none_call_accepted(all_tests_passed)
    call test_undeclared_call_in_loop_rejected(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All reject-scope-01 tests passed'
        stop 0
    else
        print *, 'Some reject-scope-01 tests failed'
        stop 1
    end if

contains

    subroutine test_undeclared_call_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Undeclared CALL under implicit none (external) (rejected)...'
        source = 'program use_missing'//new_line('a')// &
            'implicit none (type, external)'//new_line('a')// &
            'call missing_sub(1)'//new_line('a')// &
            'end program use_missing'
        call expect_frontend_error(source, "'missing_sub'", passed)
    end subroutine test_undeclared_call_rejected

    subroutine test_interface_declared_call_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'CALL with an explicit interface (accepted)...'
        source = 'program use_declared'//new_line('a')// &
            'implicit none (type, external)'//new_line('a')// &
            'interface'//new_line('a')// &
            'subroutine known_sub(n)'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'integer, intent(in) :: n'//new_line('a')// &
            'end subroutine known_sub'//new_line('a')// &
            'end interface'//new_line('a')// &
            'call known_sub(1)'//new_line('a')// &
            'end program use_declared'
        call expect_frontend_success(source, passed)
    end subroutine test_interface_declared_call_accepted

    subroutine test_external_declared_call_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'CALL to an EXTERNAL declared procedure (accepted)...'
        source = 'program use_external'//new_line('a')// &
            'implicit none (type, external)'//new_line('a')// &
            'external :: known_sub'//new_line('a')// &
            'call known_sub(1)'//new_line('a')// &
            'end program use_external'
        call expect_frontend_success(source, passed)
    end subroutine test_external_declared_call_accepted

    subroutine test_intrinsic_subroutine_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'CALL to an intrinsic subroutine (accepted)...'
        source = 'program use_intrinsic'//new_line('a')// &
            'implicit none (type, external)'//new_line('a')// &
            'real :: r'//new_line('a')// &
            'call random_number(r)'//new_line('a')// &
            'print *, r'//new_line('a')// &
            'end program use_intrinsic'
        call expect_frontend_success(source, passed)
    end subroutine test_intrinsic_subroutine_accepted

    subroutine test_contained_procedure_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'CALL to a contained procedure (accepted)...'
        source = 'program use_contained'//new_line('a')// &
            'implicit none (type, external)'//new_line('a')// &
            'call helper()'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine helper()'//new_line('a')// &
            'print *, 1'//new_line('a')// &
            'end subroutine helper'//new_line('a')// &
            'end program use_contained'
        call expect_frontend_success(source, passed)
    end subroutine test_contained_procedure_accepted

    subroutine test_plain_implicit_none_call_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Undeclared CALL under plain implicit none (accepted)...'
        source = 'program plain_none'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'call missing_sub(1)'//new_line('a')// &
            'end program plain_none'
        call expect_frontend_success(source, passed)
    end subroutine test_plain_implicit_none_call_accepted

    subroutine test_undeclared_call_in_loop_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Undeclared CALL nested in a DO loop (rejected)...'
        source = 'program nested_missing'//new_line('a')// &
            'implicit none (type, external)'//new_line('a')// &
            'integer :: i'//new_line('a')// &
            'do i = 1, 3'//new_line('a')// &
            'call missing_sub(i)'//new_line('a')// &
            'end do'//new_line('a')// &
            'end program nested_missing'
        call expect_frontend_error(source, "'missing_sub'", passed)
    end subroutine test_undeclared_call_in_loop_rejected

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

end program test_reject_scope_01_diagnostics
