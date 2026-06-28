program test_issue_1777_negative_stride
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    source = 'program test_stride' // new_line('a') // &
        '    integer :: arr(10)' // new_line('a') // &
        '    arr = [(i, i=10, 1, -2)]' // new_line('a') // &
        '    print *, arr' // new_line('a') // &
        'end program test_stride'

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, 'FAIL: unexpected error:', trim(error_msg)
        stop 1
    end if

    if (index(transformed, 'arr(5)') == 0) then
        print *, 'FAIL: dimension not corrected from arr(10) to arr(5)'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    if (index(transformed, '[(i, i=10, 1, -2)]') == 0) then
        print *, 'FAIL: implied-do with negative stride lost'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    print *, 'PASS: negative stride dimension correctly inferred'
end program test_issue_1777_negative_stride
