program test_issue_1578_block_data
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    source = "block data init_data" // new_line('a') // &
             "    implicit none" // new_line('a') // &
             "    integer :: global_val" // new_line('a') // &
             "    common /shared/ global_val" // new_line('a') // &
             "    data global_val /999/" // new_line('a') // &
             "end block data init_data" // new_line('a') // &
             "" // new_line('a') // &
             "program test_blockdata" // new_line('a') // &
             "    implicit none" // new_line('a') // &
             "    integer :: global_val" // new_line('a') // &
             "    common /shared/ global_val" // new_line('a') // &
             "" // new_line('a') // &
             "    print *, global_val" // new_line('a') // &
             "end program test_blockdata"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'block data') == 0) then
        print *, 'FAIL: BLOCK DATA construct lost'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'end block data') == 0) then
        print *, 'FAIL: END BLOCK DATA lost'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'common') == 0) then
        print *, 'FAIL: COMMON block lost'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, '!!') > 0 .or. index(output, '! common') > 0) then
        print *, 'FAIL: COMMON/DATA converted to comments instead of preserved'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'data') == 0) then
        print *, 'FAIL: DATA statement lost'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: BLOCK DATA and COMMON blocks preserved'
end program test_issue_1578_block_data
