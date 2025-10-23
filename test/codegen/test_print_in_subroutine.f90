program test_print_in_subroutine
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Print in subroutine: preserve statements ==="

    source = 'subroutine use_automatic(m)' // new_line('a') // &
             '    integer, intent(in) :: m' // new_line('a') // &
             '    real :: work(m, m)' // new_line('a') // &
             '    integer :: i, j' // new_line('a') // &
             '    ' // new_line('a') // &
             '    do i = 1, m' // new_line('a') // &
             '        do j = 1, m' // new_line('a') // &
             '            work(i, j) = real(i + j)' // new_line('a') // &
             '        end do' // new_line('a') // &
             '    end do' // new_line('a') // &
             '    ' // new_line('a') // &
             '    print *, ''Sum:'', sum(work)' // new_line('a') // &
             'end subroutine use_automatic' // new_line('a')

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.
    if (allocated(output)) then
        if (index(output, 'print *') == 0) then
            success = .false.
            print *, 'FAILED: print statement removed from subroutine'
        end if
        if (index(output, 'Sum') == 0) then
            success = .false.
            print *, 'FAILED: print argument removed'
        end if
        if (index(output, 'sum(work)') == 0) then
            success = .false.
            print *, 'FAILED: function call in print removed'
        end if
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: print statement not preserved in subroutine'
        if (allocated(output)) then
            print *, 'OUTPUT:'
            print *, trim(output)
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'ERRORS:'
                print *, trim(error_msg)
            end if
        end if
        stop 1
    end if

end program test_print_in_subroutine
