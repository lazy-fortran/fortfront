program test_do_loop_array_assignment
    ! Regression test for Issue #1271: ensure do loop bodies handle array element assignments
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    print *, "=== Testing array element assignments in do loop bodies (Issue #1271) ==="

    source = "program array_update" // new_line('a') // &
             "  implicit none" // new_line('a') // &
             "  integer :: i" // new_line('a') // &
             "  integer :: arr(5)" // new_line('a') // &
             "  arr = [1, 2, 3, 4, 5]" // new_line('a') // &
             "  do i = 1, 5" // new_line('a') // &
             "    arr(i) = arr(i) + 1" // new_line('a') // &
             "  end do" // new_line('a') // &
             "end program array_update"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, '! Unparsed') > 0) then
        print *, 'ERROR: unexpected ! Unparsed placeholder emitted'
        stop 1
    end if

    if (index(output, 'arr(i) = arr(i) + 1') == 0 .and. &
        index(output, 'arr(i)=arr(i)+1') == 0) then
        print *, 'ERROR: array assignment missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: parser keeps array element assignments intact inside do loops'
    stop 0
end program test_do_loop_array_assignment
