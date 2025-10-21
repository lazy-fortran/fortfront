! Test implied DO in array constructor
program test_issue_1575_implied_do_array_constructor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    source = 'program issue_1575' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    integer :: arr(10)' // new_line('a') // &
             '    integer :: i' // new_line('a') // &
             '' // new_line('a') // &
             '    arr = [(i*2, i=1,10)]' // new_line('a') // &
             '' // new_line('a') // &
             '    print *, arr' // new_line('a') // &
             'end program issue_1575'

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error:', trim(error_msg)
            stop 1
        end if
    end if

    if (index(transformed, 'arr = (/(i*2, i=1, 10) /)') == 0) then
        print *, 'FAIL: implied-do constructor lost'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    if (index(transformed, 'print *, arr') == 0) then
        print *, 'FAIL: print statement missing'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    if (index(transformed, 'i=1, 10') == 0) then
        print *, 'FAIL: loop bounds missing'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    print *, 'PASS: implied-do array constructor preserved'
end program test_issue_1575_implied_do_array_constructor
