program test_issue_1897_typed_array_constructor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    source = 'program typed_array_issue_1897' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    real :: arr1(5)' // new_line('a') // &
             '    integer :: arr2(3)' // new_line('a') // &
             '' // new_line('a') // &
             '    arr1 = (/ real :: 1, 2, 3, 4, 5 /)' // new_line('a') // &
             '    arr2 = (/ integer :: 1.5, 2.7, 3.9 /)' // new_line('a') // &
             '' // new_line('a') // &
             '    print *, ''Real array:'', arr1' // new_line('a') // &
             '    print *, ''Integer array:'', arr2' // new_line('a') // &
             'end program typed_array_issue_1897'

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error:', trim(error_msg)
            stop 1
        end if
    end if

    if (index(transformed, 'real :: arr1(5)') == 0) then
        print *, 'FAIL: arr1 declaration lost or incorrect'
        print *, transformed
        stop 1
    end if

    if (index(transformed, 'integer :: arr1') /= 0) then
        print *, 'FAIL: arr1 declaration changed to integer'
        print *, transformed
        stop 1
    end if

    if (index(transformed, 'integer :: arr2(3)') == 0) then
        print *, 'FAIL: arr2 declaration lost or incorrect'
        print *, transformed
        stop 1
    end if

    if (index(transformed, 'real :: arr2') /= 0) then
        print *, 'FAIL: arr2 declaration changed to real'
        print *, transformed
        stop 1
    end if

    if (index(transformed, '(/real ::') == 0 .and. &
        index(transformed, '(/ real ::') == 0) then
        print *, 'FAIL: type spec removed from arr1 constructor'
        print *, transformed
        stop 1
    end if

    if (index(transformed, '(/integer ::') == 0 .and. &
        index(transformed, '(/ integer ::') == 0) then
        print *, 'FAIL: type spec removed from arr2 constructor'
        print *, transformed
        stop 1
    end if

    print *, 'PASS: typed array constructors preserved with correct declarations'
end program test_issue_1897_typed_array_constructor
