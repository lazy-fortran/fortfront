program test_issue_1576_common_subroutine
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    integer :: common_count

    call read_example('examples/lf/issue_1576_common_subroutine.lf', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'ERROR: ' // trim(error_msg)
            error stop 1
        end if
    end if

    common_count = count_occurrences(output, 'common')
    if (common_count < 2) then
        write (error_unit, '(A,I0)') &
            'FAIL: Expected 2 COMMON statements, found ', common_count
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    if (index(output, 'common/mydata/x,y') == 0 .and. &
        index(output, 'common /mydata/x, y') == 0) then
        write (error_unit, '(A)') &
            'FAIL: COMMON block /mydata/ not preserved correctly'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    print *, 'PASS: COMMON blocks preserved in program and subroutine'

contains

    include '../../common/read_example.inc'


    integer function count_occurrences(text, substring) result(count)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: substring
        integer :: pos
        integer :: found_pos

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
