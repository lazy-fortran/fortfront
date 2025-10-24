program test_do_concurrent_issue_1828
    use fortfront, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    print *, "Testing DO CONCURRENT support (Issue #1828)"

    source = "program test_concurrent_do" // new_line('a') // &
             "    implicit none" // new_line('a') // &
             "    integer :: i" // new_line('a') // &
             "    integer :: arr(10)" // new_line('a') // &
             "    " // new_line('a') // &
             "    do concurrent (i = 1:10)" // new_line('a') // &
             "        arr(i) = i * 2" // new_line('a') // &
             "    end do" // new_line('a') // &
             "    " // new_line('a') // &
             "    print *, arr" // new_line('a') // &
             "end program test_concurrent_do"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'do i = 1, 10') == 0 .and. &
        index(output, 'do i=1,10') == 0 .and. &
        index(output, 'do i = 1,10') == 0) then
        print *, 'ERROR: DO loop missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'end do') == 0 .and. &
        index(output, 'enddo') == 0) then
        print *, 'ERROR: END DO missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'arr(i) = i*2') == 0 .and. &
        index(output, 'arr(i)=i*2') == 0 .and. &
        index(output, 'arr(i) = i * 2') == 0) then
        print *, 'ERROR: loop body missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'i = 1:10') > 0) then
        print *, 'ERROR: invalid range syntax i = 1:10 found in output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    source = "program test_multi_concurrent_do" // new_line('a') // &
             "    implicit none" // new_line('a') // &
             "    integer :: i, j" // new_line('a') // &
             "    integer :: arr(3, 3)" // new_line('a') // &
             "    " // new_line('a') // &
             "    do concurrent (i = 1:3, j = 1:3)" // new_line('a') // &
             "        arr(i, j) = i + j" // new_line('a') // &
             "    end do" // new_line('a') // &
             "    print *, arr" // new_line('a') // &
             "end program test_multi_concurrent_do"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'do i = 1, 3') == 0 .and. &
        index(output, 'do i=1,3') == 0 .and. &
        index(output, 'do i = 1,3') == 0) then
        print *, 'ERROR: outer DO loop missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'do j = 1, 3') == 0 .and. &
        index(output, 'do j=1,3') == 0 .and. &
        index(output, 'do j = 1,3') == 0) then
        print *, 'ERROR: inner DO loop missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'do i = 1, 3, j = 1') > 0 .or. &
        index(output, 'do i=1,3,j=1') > 0) then
        print *, 'ERROR: invalid combined loop header found'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'i = 1:3') > 0 .or. &
        index(output, 'j = 1:3') > 0) then
        print *, 'ERROR: invalid range syntax retained in output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'arr(i, j) = i + j') == 0 .and. &
        index(output, 'arr(i,j)=i+j') == 0 .and. &
        index(output, 'arr(i,j) = i + j') == 0) then
        print *, 'ERROR: nested loop body missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: DO CONCURRENT converted to regular DO loops'
    stop 0
end program test_do_concurrent_issue_1828
