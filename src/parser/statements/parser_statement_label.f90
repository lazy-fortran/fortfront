module parser_statement_label_module
    ! Statement label validation for free source form.
    !
    ! Fortran 2023 clause 6.2.5: a statement label is a sequence of one to five
    ! digits, at least one of which is nonzero.  Clause 6.3.2.3 requires the
    ! label to be followed, on the same line, by the statement it labels, and
    ! the label must be separated from that statement by a blank.
    use lexer_core, only: token_t, TK_EOF, TK_NEWLINE, TK_COMMENT, TK_OPERATOR
    implicit none
    private

    integer, parameter, public :: MAX_STATEMENT_LABEL_DIGITS = 5

    public :: validate_statement_label

contains

    ! Validate the leading numeric token of a statement as a statement label.
    ! next_token is the token that directly follows the label in the raw token
    ! stream; a TK_EOF, TK_NEWLINE or TK_COMMENT there means the label has no
    ! statement.  message is allocated only when the label is invalid, so an
    ! unallocated message means "accept".
    subroutine validate_statement_label(label_token, next_token, message)
        type(token_t), intent(in) :: label_token
        type(token_t), intent(in) :: next_token
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: digits

        if (.not. allocated(label_token%text)) return
        digits = trim(label_token%text)
        if (.not. is_digit_string(digits)) return

        if (len(digits) > MAX_STATEMENT_LABEL_DIGITS) then
            message = "Too many digits in statement label '"//digits//"'"
            return
        end if

        if (is_all_zero(digits)) then
            message = "Zero is not a valid statement label"
            return
        end if

        if (terminates_statement(next_token)) then
            message = "Statement label without statement"
            return
        end if

        if (label_runs_into_next_token(label_token, next_token, len(digits))) then
            message = "Invalid character in statement label field"
        end if
    end subroutine validate_statement_label

    ! A label candidate is a nonempty run of decimal digits.  Anything else
    ! (real literals, kind suffixes, signs) is not a label and is left alone.
    logical function is_digit_string(text) result(is_digits)
        character(len=*), intent(in) :: text
        integer :: i

        is_digits = .false.
        if (len(text) == 0) return
        do i = 1, len(text)
            if (text(i:i) < '0') return
            if (text(i:i) > '9') return
        end do
        is_digits = .true.
    end function is_digit_string

    logical function is_all_zero(digits) result(all_zero)
        character(len=*), intent(in) :: digits
        integer :: i

        all_zero = .true.
        do i = 1, len(digits)
            if (digits(i:i) /= '0') then
                all_zero = .false.
                return
            end if
        end do
    end function is_all_zero

    logical function terminates_statement(next_token) result(terminates)
        type(token_t), intent(in) :: next_token

        select case (next_token%kind)
        case (TK_EOF, TK_NEWLINE, TK_COMMENT)
            terminates = .true.
        case default
            terminates = .false.
        end select
    end function terminates_statement

    ! Free source form demands a blank between the label and the statement.
    ! Only the narrow case of an operator glued to the label is rejected here:
    ! that is what `10: a = 10` produces, and it cannot be a valid statement.
    logical function label_runs_into_next_token(label_token, next_token, &
            digit_count) result(runs_into)
        type(token_t), intent(in) :: label_token
        type(token_t), intent(in) :: next_token
        integer, intent(in) :: digit_count

        runs_into = .false.
        if (next_token%kind /= TK_OPERATOR) return
        if (next_token%line /= label_token%line) return
        if (next_token%column /= label_token%column + digit_count) return
        runs_into = .true.
    end function label_runs_into_next_token

end module parser_statement_label_module
