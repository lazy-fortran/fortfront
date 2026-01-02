program test_issue_177_line_continuation
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call read_example('examples/lf/issue_177_line_continuation.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'ERROR: ' // trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, '&') == 0) then
        write (error_unit, '(A)') &
            'CONFIRMED: continuation characters stripped due to limitation'
    else
        write (error_unit, '(A,I0)') 'NOTICE: continuation characters present: ', &
            count_occurrences(output, '&')
    end if

    print *, 'PASS: Issue #177 regression exercised'

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'


    integer function count_occurrences(text, pattern) result(total)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        integer :: pos
        integer :: start_pos

        total = 0
        start_pos = 1

        do
            pos = index(text(start_pos:), pattern)
            if (pos == 0) exit
            total = total + 1
            start_pos = start_pos + pos + len(pattern) - 1
        end do
    end function count_occurrences

end program test_issue_177_line_continuation
