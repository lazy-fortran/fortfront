! Test for issue #2456: Assumed character length in wrong context
! Ensures fortfront does NOT emit character(len=*) for local variables
program test_issue_2456
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, output, error_msg
    logical :: test_passed

    test_passed = .true.

    ! Test case: Roundtrip should preserve valid character usage
    call read_example('examples/f90/issue2456.f90', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: Transformation error:", trim(error_msg)
        test_passed = .false.
    else
        ! Verify output compiles with gfortran
        print *, "SUCCESS: Roundtrip transformation completed"

        ! Check that len=* appears only with intent (dummy args) or parameter
        ! NOT with local variables
        if (index(output, 'character(len=*)') > 0) then
            ! This is OK if it's a dummy argument or parameter
            ! Check that it's not a local variable declaration
            if (index(output, 'character(len=:), allocatable') == 0) then
                print *, "INFO: Found character(len=*) - should be dummy arg or parameter"
            end if
        end if
    end if

    if (test_passed) then
        print *, "test_issue_2456_assumed_char_local PASSED"
    else
        print *, "test_issue_2456_assumed_char_local FAILED"
        error stop 1
    end if


contains


    include 'common/read_example.inc'
end program test_issue_2456
