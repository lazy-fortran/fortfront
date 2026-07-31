program test_reject_placement_01_diagnostics
    ! Rejection coverage for constructs in forbidden program sections
    ! (issue #2896).
    !
    ! Rule under test: F2018 C858. The PROTECTED attribute may appear only in
    ! the specification part of a module. A main program has no module to
    ! protect anything from, so the attribute is invalid there.
    ! Represented by the constraint of gfortran.dg/pr68054.f90.
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== Reject misplaced declarations (issue #2896) ==='

    call test_protected_in_main_program_rejected(all_tests_passed)
    call test_protected_in_implicit_main_rejected(all_tests_passed)
    call test_protected_in_multi_declaration_rejected(all_tests_passed)
    call test_protected_in_module_accepted(all_tests_passed)
    call test_plain_declaration_in_main_program_accepted(all_tests_passed)
    call test_save_in_main_program_accepted(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All declaration placement rejection tests passed'
        stop 0
    else
        print *, 'Some declaration placement rejection tests failed'
        stop 1
    end if

contains

    subroutine test_protected_in_main_program_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing PROTECTED in an explicit main program (rejected)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'real, protected :: x'//new_line('a')// &
            'x = 1'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, 'PROTECTED attribute', passed)
    end subroutine test_protected_in_main_program_rejected

    subroutine test_protected_in_implicit_main_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing PROTECTED in an implicit main program (rejected)...'
        source = 'real, protected :: x'//new_line('a')// &
            'x = 1'//new_line('a')// &
            'end'
        call expect_frontend_error(source, 'specification part of a module', &
            passed)
    end subroutine test_protected_in_implicit_main_rejected

    subroutine test_protected_in_multi_declaration_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing PROTECTED on a multi declaration (rejected)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'real, protected :: x, y'//new_line('a')// &
            'x = 1'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, 'PROTECTED attribute', passed)
    end subroutine test_protected_in_multi_declaration_rejected

    subroutine test_protected_in_module_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing PROTECTED in a module specification part (accepted)...'
        source = 'module m'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'real, protected :: x'//new_line('a')// &
            'end module m'
        call expect_frontend_accepts(source, passed)
    end subroutine test_protected_in_module_accepted

    subroutine test_plain_declaration_in_main_program_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing a plain declaration in a main program (accepted)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'real :: x'//new_line('a')// &
            'x = 1'//new_line('a')// &
            'end program p'
        call expect_frontend_accepts(source, passed)
    end subroutine test_plain_declaration_in_main_program_accepted

    subroutine test_save_in_main_program_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing a SAVE declaration in a main program (accepted)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'real, save :: x'//new_line('a')// &
            'x = 1'//new_line('a')// &
            'end program p'
        call expect_frontend_accepts(source, passed)
    end subroutine test_save_in_main_program_accepted

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
        if (index(result%error_msg, expected) == 0) then
            print *, '  FAIL: diagnostic missing expected text: ', expected
            print *, trim(result%error_msg)
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
            if (allocated(result%error_msg)) print *, trim(result%error_msg)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_accepts

end program test_reject_placement_01_diagnostics
