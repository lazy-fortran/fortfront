module codegen_basic_utils
    implicit none
    private
    
    public :: add_line_continuations

contains

    ! Add line continuations for overly long lines
    function add_line_continuations(input_code) result(output_code)
        character(len=*), intent(in) :: input_code
        character(len=:), allocatable :: output_code
        integer, parameter :: MAX_LINE_LENGTH = 90
        integer, parameter :: CONTINUATION_INDENT = 6
        integer :: pos, line_start, line_end, len_input
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
        
        do while (pos <= len_input)
            ! Find the end of the current line
            line_start = pos
            line_end = pos
            do while (line_end <= len_input .and. &
                     input_code(line_end:line_end) /= char(10) .and. &
                     input_code(line_end:line_end) /= char(13))
                line_end = line_end + 1
            end do
            
            ! Extract current line (without newline)
            if (line_end > line_start) then
                current_line = input_code(line_start:line_end-1)
            else
                current_line = ""
            end if
            
            ! Check if line needs continuation
            if (len(current_line) > MAX_LINE_LENGTH) then
                call add_line_with_continuation(current_line, output_code)
            else
                output_code = output_code // current_line // new_line('A')
            end if
            
            ! Skip newline character(s)
            if (line_end <= len_input) then
                if (input_code(line_end:line_end) == char(13) .and. &
                    line_end + 1 <= len_input .and. &
                    input_code(line_end+1:line_end+1) == char(10)) then
                    pos = line_end + 2  ! CRLF
                else
                    pos = line_end + 1  ! LF or CR
                end if
            else
                pos = line_end + 1
            end if
        end do
    end function add_line_continuations

    ! Helper subroutine to add continuation to a long line
    subroutine add_line_with_continuation(input_line, output_code)
        character(len=*), intent(in) :: input_line
        character(len=:), allocatable, intent(inout) :: output_code
        integer, parameter :: MAX_LINE_LENGTH = 90
        integer, parameter :: CONTINUATION_INDENT = 6
        integer :: pos, last_break, len_line
        character(len=:), allocatable :: current_line, continuation_str
        logical :: found_break
        
        len_line = len(input_line)
        if (len_line <= MAX_LINE_LENGTH) then
            output_code = output_code // input_line // new_line('A')
            return
        end if
        
        ! Create continuation indent
        continuation_str = repeat(' ', CONTINUATION_INDENT) // '& '
        
        pos = 1
        do while (pos <= len_line)
            last_break = pos
            found_break = .false.
            
            ! Find a good break point within MAX_LINE_LENGTH
            do while (last_break - pos + 1 <= MAX_LINE_LENGTH .and. last_break <= len_line)
                if (input_line(last_break:last_break) == ' ' .or. &
                    input_line(last_break:last_break) == ',' .or. &
                    input_line(last_break:last_break) == '(' .or. &
                    input_line(last_break:last_break) == ')') then
                    found_break = .true.
                end if
                last_break = last_break + 1
            end do
            
            if (.not. found_break .or. last_break > len_line) then
                ! No good break point found, just break at MAX_LINE_LENGTH
                last_break = min(pos + MAX_LINE_LENGTH - 1, len_line)
            else
                last_break = last_break - 1  ! Step back to the break character
            end if
            
            ! Extract the line segment
            if (last_break >= pos) then
                current_line = input_line(pos:last_break)
                if (pos == 1) then
                    output_code = output_code // current_line // ' &' // new_line('A')
                else
                    output_code = output_code // continuation_str // current_line
                    if (last_break < len_line) then
                        output_code = output_code // ' &'
                    end if
                    output_code = output_code // new_line('A')
                end if
            else
                output_code = output_code // current_line // new_line('A')
            end if
            
            pos = last_break + 1
        end do
    end subroutine add_line_with_continuation


end module codegen_basic_utils