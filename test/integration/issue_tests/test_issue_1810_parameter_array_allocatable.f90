program test_issue_1810_parameter_array_allocatable
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed
    character(len=:), allocatable :: source, output, error_msg

    all_passed = .true.

    print *, '=== Issue #1810: Parameter arrays incorrectly get ALLOCATABLE ==='

    source = 'program test' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    integer, parameter :: n = 3' // new_line('a') // &
             '    real, parameter :: values(n) = [1.0, 2.0, 3.0]' // new_line('a') // &
             '    integer, parameter :: matrix(2,2) = reshape([1,2,3,4], [2,2])' // &
             new_line('a') // &
             '    print *, values' // new_line('a') // &
             '    print *, matrix' // new_line('a') // &
             'end program test'

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

    ! Check that dimensions are preserved
    if (index(output, 'values(') > 0 .or. index(output, 'values :') > 0) then
        print *, '  PASS: Array dimensions present'
    else
        print *, '  FAIL: Array dimensions lost'
        all_passed = .false.
    end if

    print *
    if (all_passed) then
        print *, 'Issue #1810 fixed!'
    else
        print *, 'Issue #1810 test failed!'
        stop 1
    end if

end program test_issue_1810_parameter_array_allocatable
