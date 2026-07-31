module parser_label_validation_module
    ! Statement label validation (Fortran 2003 R313 / C304).
    !
    ! A statement label is one to five digits, at least one of them nonzero,
    ! and in free form it is separated from the statement by blanks.  A label
    ! must be attached to a statement.  Violations are recorded here and
    ! surfaced by the parsing driver as a source diagnostic.
    implicit none
    private

    public :: validate_statement_label
    public :: validate_label_context
    public :: record_statement_label_error
    public :: is_statement_label_text
    public :: reset_statement_label_error
    public :: has_statement_label_error
    public :: get_statement_label_message

    logical :: statement_label_error = .false.
    character(len=:), allocatable :: statement_label_message

contains

    ! Check the digits of a statement label. Returns .true. when valid,
    ! otherwise .false. and an allocated rule-specific message.
    logical function validate_statement_label(label_text, message) result(valid)
        character(len=*), intent(in) :: label_text
        character(len=:), allocatable, intent(out) :: message
        character(len=:), allocatable :: digits
        integer :: i

        valid = .true.
        digits = trim(adjustl(label_text))

        do i = 1, len(digits)
            if (digits(i:i) < '0' .or. digits(i:i) > '9') then
                valid = .false.
                message = "Invalid statement label '"//digits// &
                    "': statement labels consist of digits only"
                return
            end if
        end do

        if (len(digits) == 0) return

        if (len(digits) > 5) then
            valid = .false.
            message = "Too many digits in statement label '"//digits// &
                "': at most five digits are allowed"
            return
        end if

        if (verify(digits, '0') == 0) then
            valid = .false.
            message = "Zero is not a valid statement label"
        end if
    end function validate_statement_label

    ! True when the token text can only be a statement label (digits only).
    logical function is_statement_label_text(text) result(is_label)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: digits
        integer :: i

        digits = trim(adjustl(text))
        is_label = len(digits) > 0
        if (.not. is_label) return
        do i = 1, len(digits)
            if (digits(i:i) < '0' .or. digits(i:i) > '9') then
                is_label = .false.
                return
            end if
        end do
    end function is_statement_label_text

    ! Validate a label token together with what follows it. `has_statement`
    ! reports whether a statement is attached, `colon_follows` whether the
    ! label is directly followed by a colon (an invalid separator).
    subroutine validate_label_context(label_text, has_statement, colon_follows, &
            line, column)
        character(len=*), intent(in) :: label_text
        logical, intent(in) :: has_statement
        logical, intent(in) :: colon_follows
        integer, intent(in) :: line
        integer, intent(in) :: column
        character(len=:), allocatable :: message

        if (.not. is_statement_label_text(label_text)) return

        if (.not. validate_statement_label(label_text, message)) then
            call record_statement_label_error(message, line, column)
            return
        end if

        if (colon_follows) then
            call record_statement_label_error( &
                "Invalid character ':' after statement label '"// &
                trim(adjustl(label_text))//"'", line, column)
            return
        end if

        if (.not. has_statement) then
            call record_statement_label_error( &
                "Statement label without statement", line, column)
        end if
    end subroutine validate_label_context

    subroutine record_statement_label_error(message, line, column)
        character(len=*), intent(in) :: message
        integer, intent(in) :: line
        integer, intent(in) :: column
        character(len=32) :: position_text

        if (statement_label_error) return
        statement_label_error = .true.
        write (position_text, '(I0,A,I0)') line, ':', column
        statement_label_message = trim(position_text)//": error: "//trim(message)
    end subroutine record_statement_label_error

    subroutine reset_statement_label_error()
        statement_label_error = .false.
        if (allocated(statement_label_message)) deallocate (statement_label_message)
    end subroutine reset_statement_label_error

    logical function has_statement_label_error() result(has_error)
        has_error = statement_label_error
    end function has_statement_label_error

    function get_statement_label_message() result(message)
        character(len=:), allocatable :: message

        if (allocated(statement_label_message)) then
            message = statement_label_message
        else
            message = "Invalid statement label"
        end if
    end function get_statement_label_message

end module parser_label_validation_module
