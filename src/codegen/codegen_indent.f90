module codegen_indent
    ! Module to handle indentation in code generation
    implicit none
    private

    ! Default constants
    integer, parameter :: DEFAULT_INDENT_SIZE = 4
    character(len=1), parameter :: DEFAULT_INDENT_CHAR = ' '
    integer, parameter :: DEFAULT_LINE_LENGTH = 130

    ! Module state for current indentation level and configuration
    integer, save :: current_indent_level = 0
    integer, save :: active_indent_size = DEFAULT_INDENT_SIZE
    character(len=1), save :: active_indent_char = DEFAULT_INDENT_CHAR
    integer, save :: active_line_length = DEFAULT_LINE_LENGTH

    ! Public interface
    public :: get_indent, increase_indent, decrease_indent, reset_indent
    public :: with_indent, indent_lines
    public :: set_indent_config, get_indent_config
    public :: set_line_length_config, get_line_length_config

contains

    ! Get current indentation string
    function get_indent() result(indent)
        character(len=:), allocatable :: indent
        indent = repeat(active_indent_char, current_indent_level*active_indent_size)
    end function get_indent

    ! Increase indentation level
    subroutine increase_indent()
        current_indent_level = current_indent_level + 1
    end subroutine increase_indent

    ! Decrease indentation level
    subroutine decrease_indent()
        if (current_indent_level > 0) then
            current_indent_level = current_indent_level - 1
        end if
    end subroutine decrease_indent

    ! Reset indentation to zero
    subroutine reset_indent()
        current_indent_level = 0
    end subroutine reset_indent

    ! Add current indentation to a string
    function with_indent(str) result(indented)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: indented
        indented = get_indent()//str
    end function with_indent

    ! Indent all lines in a multi-line string
    function indent_lines(text, extra_indent) result(indented)
        character(len=*), intent(in) :: text
        integer, intent(in), optional :: extra_indent
        character(len=:), allocatable :: indented
        character(len=:), allocatable :: line, indent_str
        integer :: i, start_pos, end_pos, extra

        extra = 0
        if (present(extra_indent)) extra = extra_indent

        indent_str = repeat(active_indent_char, &
                           (current_indent_level + extra)*active_indent_size)
        indented = ""
        start_pos = 1

        do i = 1, len(text)
            if (text(i:i) == new_line('a')) then
                end_pos = i - 1
                if (end_pos >= start_pos) then
                    line = text(start_pos:end_pos)
                    if (len_trim(line) > 0) then
                        indented = indented//indent_str//line//new_line('a')
                    else
                        indented = indented//new_line('a')
                    end if
                else
                    indented = indented//new_line('a')
                end if
                start_pos = i + 1
            end if
        end do

        ! Handle last line if no trailing newline
        if (start_pos <= len(text)) then
            line = text(start_pos:)
            if (len_trim(line) > 0) then
                indented = indented//indent_str//line
            end if
        end if
    end function indent_lines

    ! Set indentation configuration
    subroutine set_indent_config(size, char)
        integer, intent(in) :: size
        character(len=1), intent(in) :: char
        active_indent_size = size
        active_indent_char = char
    end subroutine set_indent_config

    ! Get current indentation configuration
    subroutine get_indent_config(size, char)
        integer, intent(out) :: size
        character(len=1), intent(out) :: char
        size = active_indent_size
        char = active_indent_char
    end subroutine get_indent_config

    ! Set line length configuration
    subroutine set_line_length_config(length)
        integer, intent(in) :: length
        active_line_length = length
    end subroutine set_line_length_config

    ! Get current line length configuration
    subroutine get_line_length_config(length)
        integer, intent(out) :: length
        length = active_line_length
    end subroutine get_line_length_config

end module codegen_indent