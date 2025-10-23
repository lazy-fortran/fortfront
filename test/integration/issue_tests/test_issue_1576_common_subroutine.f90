program test_issue_1576_common_subroutine
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    integer :: common_count

    source = "program test" // new_line('a') // &
             "    common /mydata/ x, y" // new_line('a') // &
             "    x = 1.0" // new_line('a') // &
             "    y = 2.0" // new_line('a') // &
             "    call print_data()" // new_line('a') // &
             "end program test" // new_line('a') // &
             "" // new_line('a') // &
             "subroutine print_data()" // new_line('a') // &
             "    common /mydata/ x, y" // new_line('a') // &
             "    print *, x, y" // new_line('a') // &
             "end subroutine"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    common_count = count_occurrences(output, 'common')
    if (common_count < 2) then
        print *, 'FAIL: Expected 2 COMMON statements, found', common_count
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'common/mydata/x,y') == 0 .and. &
        index(output, 'common /mydata/ x, y') == 0) then
        print *, 'FAIL: COMMON block /mydata/ not preserved correctly'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: COMMON blocks preserved in program and subroutine'

contains
    integer function count_occurrences(text, substring) result(count)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: substring
        integer :: pos, found_pos

        count = 0
        pos = 1
        do while (pos <= len(text))
            found_pos = index(text(pos:), substring)
            if (found_pos == 0) exit
            count = count + 1
            pos = pos + found_pos + len(substring) - 1
        end do
    end function count_occurrences

end program test_issue_1576_common_subroutine
