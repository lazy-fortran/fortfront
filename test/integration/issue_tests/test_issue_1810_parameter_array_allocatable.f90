program test_issue_1810_parameter_array_allocatable
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    character(len=:), allocatable :: source, output, error_msg

    all_passed = .true.

    print *, '=== Issue #1810: Parameter arrays incorrectly get ALLOCATABLE ==='

    call read_example('examples/f90/issue_1810_parameter_array_allocatable.f90', &
        source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            all_passed = .false.
        end if
    end if

    print *, 'Output:'
    print *, trim(output)
    print *

    ! Check that values does NOT have allocatable
    if (index(output, 'values') > 0) then
        if (index(output, 'allocatable') > 0 .and. &
            index(output, 'parameter') > 0) then
            print *, '  FAIL: Parameter array has ALLOCATABLE attribute'
            all_passed = .false.
        else
            print *, '  PASS: Parameter arrays do not have ALLOCATABLE'
        end if
    else
        print *, '  FAIL: values declaration missing'
        all_passed = .false.
    end if

    ! Check that explicit dimensions are preserved (no deferred shape)
    if (index(output, 'values(:)') > 0) then
        print *, '  FAIL: Parameter array uses deferred shape'
        all_passed = .false.
    else if (index(output, 'values(n)') > 0) then
        print *, '  PASS: Explicit parameter dimensions preserved'
    else
        print *, '  FAIL: Parameter array dimension missing'
        all_passed = .false.
    end if

    print *
    if (all_passed) then
        print *, 'Issue #1810 fixed!'
    else
        print *, 'Issue #1810 test failed!'
        stop 1
    end if


contains


    include '../../common/read_example.inc'
end program test_issue_1810_parameter_array_allocatable
