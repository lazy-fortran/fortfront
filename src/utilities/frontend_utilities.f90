module frontend_utilities
    ! fortfront - Utility functions module
    ! Contains helper functions and utilities

    use string_utils_mod, only: int_to_string
    use lexer_core, only: token_t, TK_COMMENT, TK_KEYWORD, TK_NEWLINE, TK_OPERATOR, &
                          TK_WHITESPACE, to_lower
    implicit none
    private

    public :: write_output_file, int_to_str, is_type_start

contains

    ! Write output to file
    subroutine write_output_file(filename, content, error_msg)
        character(len=*), intent(in) :: filename, content
        character(len=*), intent(out) :: error_msg

        integer :: unit, iostat
        character(len=:), allocatable :: sanitized

        open (newunit=unit, file=filename, status='replace', action='write', &
              iostat=iostat)
        if (iostat /= 0) then
            error_msg = "Cannot create output file: " // filename
            return
        end if

        sanitized = strip_space_before_comma(content)
        write (unit, '(A)') sanitized
        close (unit)
        error_msg = ""
    end subroutine write_output_file

    function strip_space_before_comma(text) result(clean)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: clean
        character(len=len(text)) :: buffer
        integer :: i, pos

        pos = 0
        do i = 1, len(text)
            if (text(i:i) == ',' .and. pos > 0) then
                if (buffer(pos:pos) == ' ') pos = pos - 1
            end if
            pos = pos + 1
            buffer(pos:pos) = text(i:i)
        end do

        if (pos > 0) then
            clean = buffer(1:pos)
        else
            clean = ''
        end if
    end function strip_space_before_comma

    ! Helper function to convert integer to string
    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=20) :: str

        str = int_to_string(num)
    end function int_to_str

    ! Check if token sequence starts a type definition
    pure logical function is_type_start(tokens, pos) result(is_start)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: pos
        integer :: next_idx, prev_idx
        character(len=:), allocatable :: lowered

        is_start = .false.
        if (pos < 1 .or. pos > size(tokens)) return

        if (tokens(pos)%kind /= TK_KEYWORD) return
        lowered = to_lower(trim(tokens(pos)%text))
        if (lowered /= "type") return

        prev_idx = pos - 1
        do while (prev_idx >= 1)
            select case (tokens(prev_idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                prev_idx = prev_idx - 1
            case default
                exit
            end select
        end do

        if (prev_idx >= 1) then
            if (tokens(prev_idx)%kind == TK_KEYWORD) then
                lowered = to_lower(trim(tokens(prev_idx)%text))
                if (lowered == "end" .or. lowered == "select") return
            end if
        end if

        next_idx = pos + 1
        do while (next_idx <= size(tokens))
            select case (tokens(next_idx)%kind)
            case (TK_WHITESPACE, TK_COMMENT, TK_NEWLINE)
                next_idx = next_idx + 1
            case default
                exit
            end select
        end do

        if (next_idx <= size(tokens)) then
            if (tokens(next_idx)%kind == TK_OPERATOR) then
                if (tokens(next_idx)%text == "(") return
            else if (tokens(next_idx)%kind == TK_KEYWORD) then
                lowered = to_lower(trim(tokens(next_idx)%text))
                if (lowered == "is") return
            end if
        end if

        is_start = .true.
    end function is_type_start

end module frontend_utilities
