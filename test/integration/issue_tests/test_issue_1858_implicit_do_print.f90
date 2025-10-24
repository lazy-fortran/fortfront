program test_issue_1858_implicit_do_print
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    source = 'print *, (i**2, i=1,5)'

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error:', trim(error_msg)
            stop 1
        end if
    end if

    if (index(transformed, 'integer :: i') == 0) then
        print *, 'FAIL: implicit DO loop variable not declared'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    if (index(transformed, 'print *, (i**2, i = 1, 5)') == 0 .and. &
        index(transformed, 'print *, (i**2, i=1, 5)') == 0) then
        print *, 'FAIL: implicit DO statement malformed'
        print *, 'Output:'
        print *, trim(transformed)
        stop 1
    end if

    print *, 'PASS: implicit DO loop variable declared correctly'
end program test_issue_1858_implicit_do_print
