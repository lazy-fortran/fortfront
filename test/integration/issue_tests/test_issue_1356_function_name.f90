program test_issue_1356_function_name
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    use lexer_core, only: to_lower
    implicit none

    character(len=:), allocatable :: input_text
    character(len=:), allocatable :: output_text
    character(len=:), allocatable :: lower_output_text
    character(len=:), allocatable :: error_msg

    call read_example('examples/lf/issue_1356_function_name.lf', input_text)

    call transform_lazy_fortran_string(input_text, output_text, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: transformation reported error:'
            write (error_unit, '(A)') trim(error_msg)
            error stop 1
        end if
    end if

    if (.not. allocated(output_text)) then
        write (error_unit, '(A)') 'FAIL: no output produced for issue_1356 example'
        error stop 1
    end if

    lower_output_text = to_lower(output_text)

    if (index(lower_output_text, 'integer function double') == 0) then
        write (error_unit, '(A)') &
            'FAIL: function double is not emitted with integer return type'
        write (error_unit, '(A)') trim(output_text)
        error stop 1
    end if

    if (index(lower_output_text, 'integer, intent(in) :: x') == 0) then
        write (error_unit, '(A)') 'FAIL: parameter x is not inferred as integer'
        write (error_unit, '(A)') trim(output_text)
        error stop 1
    end if

    if (.not. has_integer_declaration(lower_output_text, [character(len=16) :: &
                                                          'a', 'b'])) then
        write (error_unit, '(A)') &
            'FAIL: caller variables a/b lack inferred integer declarations'
        write (error_unit, '(A)') trim(output_text)
        error stop 1
    end if

    if (index(lower_output_text, 'real function double') > 0 .or. &
        index(lower_output_text, 'real :: double') > 0) then
        write (error_unit, '(A)') 'FAIL: real declarations for double remain'
        write (error_unit, '(A)') trim(output_text)
        error stop 1
    end if

    print *, 'PASS: issue_1356 function inference retains integer types'

contains

    include '../../common/cli_io_reader.inc'
    include '../../common/read_example.inc'


    logical function has_integer_declaration(text, names)
        character(len=*), intent(in) :: text
        character(len=*), dimension(:), intent(in) :: names
        integer :: pos
        integer :: start_pos
        integer :: end_pos
        integer :: i
        integer :: text_len
        character(len=:), allocatable :: line
        character(1), parameter :: nl = new_line('a')

        has_integer_declaration = .false.
        text_len = len(text)
        pos = index(text, 'integer ::')

        do while (pos > 0)
            start_pos = pos
            do while (start_pos > 1 .and. text(start_pos - 1:start_pos - 1) /= nl)
                start_pos = start_pos - 1
            end do

            end_pos = pos
            do while (end_pos <= text_len .and. text(end_pos:end_pos) /= nl)
                end_pos = end_pos + 1
            end do

            if (end_pos > text_len) then
                line = text(start_pos:)
            else
                line = text(start_pos:end_pos - 1)
            end if

            line = adjustl(line)
            if (index(line, 'integer ::') == 1) then
                has_integer_declaration = .true.
                do i = 1, size(names)
                    if (index(line, trim(names(i))) == 0) then
                        has_integer_declaration = .false.
                        exit
                    end if
                end do
                if (has_integer_declaration) return
            end if

            if (end_pos > text_len) exit
            pos = index(text(end_pos:), 'integer ::')
            if (pos > 0) pos = pos + end_pos - 1
        end do
    end function has_integer_declaration

end program test_issue_1356_function_name
