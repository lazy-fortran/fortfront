program test_issue_1536_array_stride_assignment
    ! Regression test for issue #1536: array section assignment with stride
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    source = "integer :: arr(10) = [1, 2, 3, 4, 5, 6, 7, 8," // &
             " 9, 10]" // new_line('a') // &
             "arr(1:9:2) = 0" // new_line('a') // &
             "print *, arr"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'arr(1:9:2) = 0') > 0 .or. &
        index(output, 'arr(1:9:2)=0') > 0) then
        print *, 'PASS: array stride assignment preserved'
    else
        print *, 'FAIL: array stride assignment missing in output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if
end program test_issue_1536_array_stride_assignment
