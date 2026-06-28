program test_issue_1547_scalar_allocatable
    ! Test for issue #1547: Scalars incorrectly marked as allocatable
    use fortfront, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1547 Scalar Allocatable Tests ==='
    print *

    ! Test that scalars are not marked allocatable due to multiple assignments
    if (.not. test_scalar_multiple_assignments()) all_passed = .false.
    if (.not. test_scalar_single_assignment()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All scalar allocatable tests passed!'
        stop 0
    else
        print *, 'Some scalar allocatable tests failed!'
        stop 1
    end if

contains

    include '../common/read_example.inc'


    logical function test_scalar_multiple_assignments()
        test_scalar_multiple_assignments = .true.
        print *, 'Testing scalar with multiple assignments...'

        block
            character(len=:), allocatable :: input, output, error_msg

            ! Scalar with multiple assignments - should NOT be allocatable
            call read_example('examples/lf/issue_1547_scalar_multiple_assignments.lf', &
                input)

            call transform_lazy_fortran_string(input, output, error_msg)

            if (error_msg /= "") then
                print *, '  FAIL: Transformation error: ', error_msg
                test_scalar_multiple_assignments = .false.
                return
            end if

            if (.not. allocated(output)) then
                print *, '  FAIL: No output generated'
                test_scalar_multiple_assignments = .false.
                return
            end if

            ! Check that the output does NOT contain "allocatable" for the scalar
            if (index(output, "allocatable") > 0) then
                print *, '  FAIL: Scalar incorrectly marked as allocatable'
                print *, '  Output:', output
                test_scalar_multiple_assignments = .false.
                return
            end if

            ! Check that the output contains the expected declaration
            if (index(output, "integer :: x") == 0) then
                print *, '  FAIL: Expected declaration not found'
                print *, '  Output:', output
                test_scalar_multiple_assignments = .false.
                return
            end if
        end block

        print *, '  PASS: Scalar with multiple assignments handled correctly'
    end function test_scalar_multiple_assignments

    logical function test_scalar_single_assignment()
        test_scalar_single_assignment = .true.
        print *, 'Testing scalar with single assignment...'

        block
            character(len=:), allocatable :: input, output, error_msg

            ! Scalar with single assignment - should NOT be allocatable
            call read_example('examples/lf/issue_1547_scalar_single_assignment.lf', &
                input)

            call transform_lazy_fortran_string(input, output, error_msg)

            if (error_msg /= "") then
                print *, '  FAIL: Transformation error: ', error_msg
                test_scalar_single_assignment = .false.
                return
            end if

            if (.not. allocated(output)) then
                print *, '  FAIL: No output generated'
                test_scalar_single_assignment = .false.
                return
            end if

            ! Check that the output does NOT contain "allocatable" for the scalar
            if (index(output, "allocatable") > 0) then
                print *, '  FAIL: Scalar incorrectly marked as allocatable'
                print *, '  Output:', output
                test_scalar_single_assignment = .false.
                return
            end if

            ! Check that the output contains the expected declaration
            if (index(output, "integer :: x") == 0) then
                print *, '  FAIL: Expected declaration not found'
                print *, '  Output:', output
                test_scalar_single_assignment = .false.
                return
            end if
        end block

        print *, '  PASS: Scalar with single assignment handled correctly'
    end function test_scalar_single_assignment

end program test_issue_1547_scalar_allocatable
