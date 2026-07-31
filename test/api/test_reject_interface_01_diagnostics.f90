program test_reject_interface_01_diagnostics
    ! Rejection coverage for explicit-interface declaration rules (issue #2883).
    !
    ! Rule under test: F2018 C1414 (F2003 C1204). A module-procedure-stmt may
    ! appear only in a generic interface block. An ABSTRACT INTERFACE block is
    ! never generic, so MODULE PROCEDURE inside one is invalid.
    ! Represented by gfortran.dg/interface_abstract_3.f90.
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== Reject invalid interface declarations (issue #2883) ==='

    call test_module_procedure_in_abstract_interface_rejected(all_tests_passed)
    call test_module_procedure_double_colon_rejected(all_tests_passed)
    call test_module_procedure_in_generic_interface_accepted(all_tests_passed)
    call test_abstract_interface_with_subroutine_body_accepted(all_tests_passed)
    call test_abstract_interface_with_function_body_accepted(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All interface declaration rejection tests passed'
        stop 0
    else
        print *, 'Some interface declaration rejection tests failed'
        stop 1
    end if

contains

    subroutine test_module_procedure_in_abstract_interface_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing MODULE PROCEDURE in ABSTRACT INTERFACE (rejected)...'
        source = 'module m'//new_line('a')// &
            'abstract interface'//new_line('a')// &
            'module procedure p'//new_line('a')// &
            'end interface'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine p()'//new_line('a')// &
            'end subroutine p'//new_line('a')// &
            'end module m'
        call expect_frontend_error(source, 'ABSTRACT INTERFACE', passed)
    end subroutine test_module_procedure_in_abstract_interface_rejected

    subroutine test_module_procedure_double_colon_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing MODULE PROCEDURE :: in ABSTRACT INTERFACE (rejected)...'
        source = 'module m'//new_line('a')// &
            'abstract interface'//new_line('a')// &
            'module procedure :: p, q'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end module m'
        call expect_frontend_error(source, 'MODULE PROCEDURE', passed)
    end subroutine test_module_procedure_double_colon_rejected

    subroutine test_module_procedure_in_generic_interface_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing MODULE PROCEDURE in a generic interface (accepted)...'
        source = 'module m'//new_line('a')// &
            'interface gen'//new_line('a')// &
            'module procedure p'//new_line('a')// &
            'end interface gen'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine p()'//new_line('a')// &
            'end subroutine p'//new_line('a')// &
            'end module m'
        call expect_frontend_accepts(source, passed)
    end subroutine test_module_procedure_in_generic_interface_accepted

    subroutine test_abstract_interface_with_subroutine_body_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ABSTRACT INTERFACE with a subroutine body (accepted)...'
        source = 'module m'//new_line('a')// &
            'abstract interface'//new_line('a')// &
            'subroutine handler(x)'//new_line('a')// &
            'integer, intent(in) :: x'//new_line('a')// &
            'end subroutine handler'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end module m'
        call expect_frontend_accepts(source, passed)
    end subroutine test_abstract_interface_with_subroutine_body_accepted

    subroutine test_abstract_interface_with_function_body_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing ABSTRACT INTERFACE with a function body (accepted)...'
        source = 'module m'//new_line('a')// &
            'abstract interface'//new_line('a')// &
            'pure function scorer(x) result(v)'//new_line('a')// &
            'real, intent(in) :: x'//new_line('a')// &
            'real :: v'//new_line('a')// &
            'end function scorer'//new_line('a')// &
            'end interface'//new_line('a')// &
            'end module m'
        call expect_frontend_accepts(source, passed)
    end subroutine test_abstract_interface_with_function_body_accepted

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

end program test_reject_interface_01_diagnostics
