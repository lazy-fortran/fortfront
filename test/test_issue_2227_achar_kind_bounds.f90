program test_issue_2227_achar_kind_bounds
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg
    logical :: test_passed
    character(len=*), parameter :: example_path = &
        'examples/f90/issue_2227_achar_kind_crash.f90'

    test_passed = .true.

    ! Test that achar with explicit kind parameter doesn't cause bounds error
    call test_achar_kind_no_crash()

    if (test_passed) then
        print *, "test_issue_2227_achar_kind_bounds PASSED"
    else
        print *, "test_issue_2227_achar_kind_bounds FAILED"
        error stop 1
    end if

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'

    subroutine test_achar_kind_no_crash()
        call read_example(example_path, input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "ERROR: Transformation failed:", trim(error_msg)
            test_passed = .false.
            return
        end if

        ! Verify the output contains the achar call with kind parameter
        if (index(output, 'achar') == 0) then
            print *, "ERROR: Lost achar intrinsic call"
            test_passed = .false.
            return
        end if

        ! Verify the kind parameter is preserved
        if (index(output, 'kind') == 0) then
            print *, "ERROR: Lost kind parameter"
            test_passed = .false.
            return
        end if

        print *, "  - achar with kind parameter processed without crash"
    end subroutine test_achar_kind_no_crash


end program test_issue_2227_achar_kind_bounds
