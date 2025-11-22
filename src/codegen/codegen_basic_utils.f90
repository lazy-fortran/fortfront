module codegen_basic_utils
    use codegen_indent, only: get_line_length_config
    implicit none
    private

    public :: add_line_continuations

contains

    ! Add line continuations for overly long lines
    function add_line_continuations(input_code) result(output_code)
        character(len=*), intent(in) :: input_code
        character(len=:), allocatable :: output_code
        integer, parameter :: CONTINUATION_INDENT = 6
        integer :: pos, line_start, line_end, len_input
        integer :: max_len
        character(len=:), allocatable :: current_line
        logical :: in_string, in_comment
        character :: quote_char

        len_input = len(input_code)
        if (len_input == 0) then
            output_code = ""
            return
        end if

        output_code = ""
        pos = 1

        call get_line_length_config(max_len)

        do while (pos <= len_input)
            ! Find the end of the current line
            line_start = pos
            line_end = pos
            do
                if (line_end > len_input) exit
                if (input_code(line_end:line_end) == char(10) .or. &
                    input_code(line_end:line_end) == char(13)) exit
                line_end = line_end + 1
            end do

            ! Extract current line (without newline)
            if (line_end > line_start) then
                current_line = input_code(line_start:line_end - 1)
            else
                current_line = ""
            end if

            ! Check if line needs continuation
            if (len(current_line) > max_len) then
                call add_line_with_continuation(current_line, output_code, max_len)
            else
                output_code = output_code // current_line // new_line('A')
            end if

            ! Skip newline character(s)
            if (line_end <= len_input) then
                if (input_code(line_end:line_end) == char(13)) then
                    if (line_end + 1 <= len_input) then
                        if (input_code(line_end + 1:line_end + 1) == char(10)) then
                            pos = line_end + 2  ! CRLF
                        else
                            pos = line_end + 1  ! Lone CR
                        end if
                    else
                        pos = line_end + 1
                    end if
                else
                    pos = line_end + 1  ! LF
                end if
            else
                pos = line_end + 1
            end if
        end do
    end function add_line_continuations

    ! Helper subroutine to add continuation to a long line
    subroutine add_line_with_continuation(input_line, output_code, max_len)
        character(len=*), intent(in) :: input_line
        character(len=:), allocatable, intent(inout) :: output_code
        integer, intent(in) :: max_len
        integer, parameter :: CONTINUATION_INDENT = 6
        integer :: pos, last_break, len_line, i
        character(len=:), allocatable :: current_line, continuation_str
        logical :: found_break

        len_line = len(input_line)
        if (len_line <= max_len) then
            output_code = output_code // input_line // new_line('A')
            return
        end if

        ! Create continuation indent
        continuation_str = repeat(' ', CONTINUATION_INDENT) // '& '

        pos = 1
        do while (pos <= len_line)
            ! Find the last good break point within MAX_LINE_LENGTH
            ! Strategy: scan the valid range and remember the LAST break character position
            last_break = 0  ! Will hold position of last break char found
            found_break = .false.

            ! Scan from pos to min(pos+max_len-1, len_line)
            do i = pos, min(pos + max_len - 1, len_line)
                if (input_line(i:i) == ' ' .or. &
                    input_line(i:i) == ',' .or. &
                    input_line(i:i) == '(' .or. &
                    input_line(i:i) == ')') then
                    ! Remember this position - it's a valid break point
                    last_break = i
                    found_break = .true.
                end if
            end do

            ! Determine actual break position
            if (.not. found_break) then
                ! No break character found in range - must break at max_len
                ! This will split a token, but we have no choice
                last_break = min(pos + max_len - 1, len_line)
            end if

            ! Extract the line segment
            if (last_break >= pos) then
                current_line = input_line(pos:last_break)
                if (pos == 1) then
                    output_code = output_code // current_line // ' &' // new_line('A')
                else
                    output_code = output_code // continuation_str // current_line // new_line('A')
                end if
                ! Write the remainder of the line as-is and exit (single continuation is enough)
                if (last_break + 1 <= len_line) then
                    output_code = output_code // input_line(last_break + 1:len_line) // new_line('A')
                end if
                exit
            else
                output_code = output_code // input_line // new_line('A')
                exit
            end if
        end do
    end subroutine add_line_with_continuation

end module codegen_basic_utils
