module parser_state_module
    use iso_fortran_env, only: error_unit
    use lexer_core, only: token_t, TK_EOF
    use error_reporting, only: error_collection_t
    implicit none
    private

    ! Parser state type for tracking position in token stream
    type, public :: parser_state_t
        type(token_t), allocatable :: tokens(:)
        integer :: current_token = 1
        type(error_collection_t) :: errors
    contains
        procedure :: peek => parser_peek
        procedure :: consume => parser_consume
        procedure :: is_at_end => parser_is_at_end
        procedure :: match => parser_match
        procedure :: expect => parser_expect
        procedure :: error => parser_add_error
        procedure :: has_errors => parser_has_errors
        procedure :: get_error_messages => parser_get_error_messages
        procedure :: deep_copy => parser_state_deep_copy
        procedure :: assign => parser_state_assign
        generic :: assignment(=) => assign
    end type parser_state_t

    ! Public constructor
    public :: create_parser_state

contains

    ! Create parser state from tokens
    function create_parser_state(tokens) result(state)
        type(token_t), intent(in) :: tokens(:)
        type(parser_state_t) :: state

        allocate (state%tokens(size(tokens)))
        state%tokens = tokens
        state%current_token = 1
    end function create_parser_state

    ! Peek at current token without consuming it
    function parser_peek(this) result(current_token)
        class(parser_state_t), intent(in) :: this
        type(token_t) :: current_token

        if (this%current_token <= size(this%tokens)) then
            current_token = this%tokens(this%current_token)
        else
            ! Return EOF token
            current_token%kind = TK_EOF
            current_token%text = ""
            current_token%line = 1
            current_token%column = 1
        end if
    end function parser_peek

    ! Consume current token and advance
    function parser_consume(this) result(consumed_token)
        class(parser_state_t), intent(inout) :: this
        type(token_t) :: consumed_token

        consumed_token = this%peek()
        if (.not. this%is_at_end()) then
            this%current_token = this%current_token + 1
        end if
    end function parser_consume

    ! Check if we're at the end of tokens
    logical function parser_is_at_end(this)
        class(parser_state_t), intent(in) :: this
        type(token_t) :: current

        current = this%peek()
        parser_is_at_end = (current%kind == TK_EOF)
    end function parser_is_at_end

    ! Check if current token matches expected kind and consume if so
    logical function parser_match(this, expected_kind)
        class(parser_state_t), intent(inout) :: this
        integer, intent(in) :: expected_kind
        type(token_t) :: current, consumed

        current = this%peek()
        if (current%kind == expected_kind) then
            consumed = this%consume()
            parser_match = .true.
        else
            parser_match = .false.
        end if
    end function parser_match

    ! Expect a specific token kind, add error if not found
    logical function parser_expect(this, expected_kind, error_message)
        class(parser_state_t), intent(inout) :: this
        integer, intent(in) :: expected_kind
        character(len=*), intent(in), optional :: error_message
        type(token_t) :: current
        character(len=:), allocatable :: msg

        current = this%peek()
        if (current%kind == expected_kind) then
            current = this%consume()
            parser_expect = .true.
        else
            parser_expect = .false.
            if (present(error_message)) then
                msg = error_message
            else
                msg = "Unexpected token"
            end if
            call this%errors%add_error_with_token(msg, current)
        end if
    end function parser_expect

    ! Add error with current token context
    subroutine parser_add_error(this, message, suggestion)
        class(parser_state_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: suggestion
        type(token_t) :: current

        current = this%peek()
        call this%errors%add_error_with_token(message, current, suggestion=suggestion)
    end subroutine parser_add_error

    ! Check if parser has any errors
    logical function parser_has_errors(this)
        class(parser_state_t), intent(in) :: this
        parser_has_errors = this%errors%has_errors()
    end function parser_has_errors

    ! Get formatted error messages
    function parser_get_error_messages(this) result(messages)
        class(parser_state_t), intent(in) :: this
        character(len=:), allocatable :: messages
        messages = this%errors%format_messages()
    end function parser_get_error_messages

    ! Deep copy parser state
    function parser_state_deep_copy(this) result(copy)
        class(parser_state_t), intent(in) :: this
        type(parser_state_t) :: copy

        copy%current_token = this%current_token
        if (allocated(this%tokens)) then
            allocate (copy%tokens(size(this%tokens)))
            copy%tokens = this%tokens  ! token_t should have proper assignment
        end if
    end function parser_state_deep_copy

    ! Assignment operator for parser_state_t (deep copy)
    subroutine parser_state_assign(lhs, rhs)
        class(parser_state_t), intent(out) :: lhs
        type(parser_state_t), intent(in) :: rhs

        lhs%current_token = rhs%current_token
        if (allocated(rhs%tokens)) then
            allocate (lhs%tokens(size(rhs%tokens)))
            lhs%tokens = rhs%tokens  ! token_t should have proper assignment
        end if
    end subroutine parser_state_assign

end module parser_state_module
