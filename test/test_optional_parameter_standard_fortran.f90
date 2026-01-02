program test_optional_parameter_standard_fortran
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    logical :: test_passed
    character(len=*), parameter :: example_path = &
        'examples/f90/issue_2015_optional_param_wrong_monomorph.f90'

    test_passed = .true.

    ! Test standard Fortran with OPTIONAL parameter should pass through unchanged
    call test_optional_preserved()

    if (test_passed) then
        print *, "test_optional_parameter_standard_fortran PASSED"
    else
        print *, "test_optional_parameter_standard_fortran FAILED"
        error stop 1
    end if

contains

    include 'common/read_example.inc'

    subroutine test_optional_preserved()
        call read_example(example_path, input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
            return
        end if

        ! Should NOT create a module (standard Fortran should pass through)
        if (index(output, 'module auto_greet') > 0) then
            print *, "ERROR: Created monomorphized module for standard Fortran"
            test_passed = .false.
            return
        end if

        ! Should NOT create duplicate procedures
        if (index(output, 'greet__ch') > 0) then
            print *, "ERROR: Created monomorphized procedure variants"
            test_passed = .false.
            return
        end if

        ! Should preserve OPTIONAL attribute
        if (index(output, 'optional') == 0) then
            print *, "ERROR: Lost OPTIONAL attribute"
            test_passed = .false.
            return
        end if

        ! Should preserve the subroutine in contains
        if (index(output, 'subroutine greet') == 0) then
            print *, "ERROR: Lost original subroutine"
            test_passed = .false.
            return
        end if

        print *, "  - OPTIONAL parameter preserved correctly"
    end subroutine test_optional_preserved


end program test_optional_parameter_standard_fortran
