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
    ! Per ISO/IEC 1539-1:2018 Section 6.3.2.5, line breaks inside string literals
    ! require special handling. To avoid corrupting string content, this function
    ! only breaks at positions outside of string literals.
    subroutine add_line_with_continuation(input_line, output_code, max_len)
        character(len=*), intent(in) :: input_line
        character(len=:), allocatable, intent(inout) :: output_code
        integer, intent(in) :: max_len
        integer, parameter :: CONTINUATION_INDENT = 6
        integer :: i, len_line, limit, remaining, last_break
        integer :: segment_start
        character(len=:), allocatable :: continuation_str
        character :: quote_char, c
        logical :: in_string, first_segment, has_more

        len_line = len(input_line)
        if (len_line <= max_len) then
            output_code = output_code // input_line // new_line('A')
            return
        end if

        ! Create continuation indent
        continuation_str = repeat(' ', CONTINUATION_INDENT) // '& '

        segment_start = 1
        first_segment = .true.

        do
            in_string = .false.
            quote_char = ' '

            remaining = len_line - segment_start + 1
            if (remaining <= max_len) then
                if (first_segment) then
                    output_code = output_code // input_line(segment_start:) // &
                                  new_line('A')
                else
                    output_code = output_code // continuation_str // &
                                  input_line(segment_start:) // new_line('A')
                end if
                exit
            end if

            limit = min(segment_start + max_len - 1, len_line)
            last_break = 0

            i = segment_start
            do while (i <= limit)
                c = input_line(i:i)

                if (in_string) then
                    if (c == quote_char) then
                        if (i < len_line) then
                            if (input_line(i + 1:i + 1) == quote_char) then
                                i = i + 1
                            else
                                in_string = .false.
                            end if
                        else
                            in_string = .false.
                        end if
                    end if
                else
                    if (c == '"' .or. c == '''') then
                        in_string = .true.
                        quote_char = c
                    else if (c == ' ' .or. c == ',' .or. c == '(' .or. c == ')') then
                        last_break = i
                    end if
                end if

                i = i + 1
            end do

            if (last_break == 0) then
                if (in_string) then
                    output_code = output_code // input_line(segment_start:) // &
                                  new_line('A')
                    exit
                else
                    last_break = limit
                end if
            end if

            has_more = last_break < len_line

            if (first_segment) then
                output_code = output_code // &
                              input_line(segment_start:last_break)
            else
                output_code = output_code // continuation_str // &
                              input_line(segment_start:last_break)
            end if

            if (has_more) then
                output_code = output_code // ' &' // new_line('A')
            else
                output_code = output_code // new_line('A')
            end if

            if (.not. has_more) exit

            segment_start = last_break + 1
            first_segment = .false.
        end do
    end subroutine add_line_with_continuation

end module codegen_basic_utils
