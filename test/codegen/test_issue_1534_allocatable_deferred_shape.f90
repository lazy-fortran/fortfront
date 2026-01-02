program test_issue_1534_allocatable_deferred_shape
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    print *, '=== Issue #1534 Deferred Shape Allocatable Tests ==='
    all_passed = .true.

    if (.not. test_single_dimension_allocatable()) all_passed = .false.
    if (.not. test_multi_dimension_allocatable()) all_passed = .false.

    if (all_passed) then
        print *, 'All deferred shape tests passed!'
        stop 0
    else
        print *, 'Deferred shape tests failed!'
        stop 1
    end if

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'

    logical function test_single_dimension_allocatable()
        implicit none
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: ok

        test_single_dimension_allocatable = .true.
        print *, 'Testing single-dimension allocatable array...'

        call read_example('examples/lf/issue_1534_single_dimension_allocatable.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: Transformation error:', trim(error_msg)
            test_single_dimension_allocatable = .false.
            return
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No generated output'
            test_single_dimension_allocatable = .false.
            return
        end if

        ok = index(output, 'allocatable :: vec(:)') > 0 .and. &
             index(output, ':: vec(3)') == 0

        if (.not. ok) then
            print *, '  FAIL: Deferred shape not preserved'
            print *, '  Output:', trim(output)
            test_single_dimension_allocatable = .false.
            return
        end if

        if (.not. has_iso_dp_import(output)) then
            print *, '  FAIL: Missing iso_fortran_env dp import'
            test_single_dimension_allocatable = .false.
            return
        end if

        print *, '  PASS: Deferred shape preserved for single array'
    end function test_single_dimension_allocatable

    logical function test_multi_dimension_allocatable()
        implicit none
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        logical :: ok

        test_multi_dimension_allocatable = .true.
        print *, 'Testing multi-dimension allocatable array...'

        call read_example('examples/lf/issue_1534_multi_dimension_allocatable.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: Transformation error:', trim(error_msg)
            test_multi_dimension_allocatable = .false.
            return
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: No generated output'
            test_multi_dimension_allocatable = .false.
            return
        end if

        ok = index(output, 'allocatable :: grid(:,:)') > 0 .and. &
             index(output, ':: grid(2,3)') == 0

        if (.not. ok) then
            print *, '  FAIL: Deferred shape not preserved for grid'
            print *, '  Output:', trim(output)
            test_multi_dimension_allocatable = .false.
            return
        end if

        if (.not. has_iso_dp_import(output)) then
            print *, '  FAIL: Missing iso_fortran_env dp import'
            test_multi_dimension_allocatable = .false.
            return
        end if

        print *, '  PASS: Deferred shape preserved for multi array'
    end function test_multi_dimension_allocatable

    logical function has_iso_dp_import(output)
        character(len=*), intent(in) :: output

        has_iso_dp_import = index(output, 'iso_fortran_env') > 0 .and. &
                            index(output, 'dp => real64') > 0
    end function has_iso_dp_import


end program test_issue_1534_allocatable_deferred_shape
