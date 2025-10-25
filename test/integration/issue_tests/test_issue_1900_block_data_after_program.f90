program test_issue_1900_block_data_after_program
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    source = "program test_block_data" // new_line('a') // &
             "    implicit none" // new_line('a') // &
             "    integer :: a, b" // new_line('a') // &
             "    real :: x, y" // new_line('a') // &
             "    common /myblock/ a, b, x, y" // new_line('a') // &
             "" // new_line('a') // &
             "    print *, a, b" // new_line('a') // &
             "    print *, x, y" // new_line('a') // &
             "end program test_block_data" // new_line('a') // &
             "" // new_line('a') // &
             "block data init_data" // new_line('a') // &
             "    integer :: a, b" // new_line('a') // &
             "    real :: x, y" // new_line('a') // &
             "    common /myblock/ a, b, x, y" // new_line('a') // &
             "    data a, b / 10, 20 /" // new_line('a') // &
             "    data x, y / 3.5, 7.2 /" // new_line('a') // &
             "end block data init_data"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'program main') > 0) then
        print *, 'FAIL: block data wrapped inside synthetic program'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'block data init_data') == 0) then
        print *, 'FAIL: block data unit missing'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'data a, b/10, 20 /') == 0) then
        print *, 'FAIL: integer DATA statement missing'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'data x, y/3.5, 7.2 /') == 0) then
        print *, 'FAIL: real DATA statement missing'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: block data preserved after program unit'
end program test_issue_1900_block_data_after_program
