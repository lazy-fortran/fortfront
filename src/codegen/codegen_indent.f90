module codegen_indent
    ! Module to handle indentation in code generation
    implicit none
    private

    ! Constants
    integer, parameter :: INDENT_SIZE = 4
    character(len=1), parameter :: INDENT_CHAR = ' '

    ! Module state for current indentation level
    integer, save :: current_indent_level = 0

    ! Public interface
    public :: get_indent, increase_indent, decrease_indent, reset_indent
    public :: with_indent, indent_lines

contains

    ! Get current indentation string
    function get_indent() result(indent)
        character(len=:), allocatable :: indent
        indent = repeat(INDENT_CHAR, current_indent_level*INDENT_SIZE)
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

        indent_str = repeat(INDENT_CHAR, (current_indent_level + extra)*INDENT_SIZE)
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

end module codegen_indent
