module parser_state_module
    use lexer_core, only: token_t, TK_EOF
    use error_reporting, only: error_collection_t
    implicit none
    private

    ! Parser state type for tracking position in token stream.
    ! Tokens are held as a pointer: either a view into a caller-owned buffer
    ! (create_parser_state) or an owned allocation; owns_tokens distinguishes
    ! the two for cleanup and assignment.
    type, public :: parser_state_t
        ! Token storage - either view into external buffer or owned allocation
        type(token_t), pointer :: tokens(:) => null()
        logical :: owns_tokens = .false.

        ! Parser position and error tracking
        integer :: current_token = 1
        integer :: generation = 1 ! Generation for lifecycle tracking
        type(error_collection_t) :: errors
        type(error_collection_t), pointer :: diagnostic_sink => null()
    contains
        procedure :: peek => parser_peek
        procedure :: consume => parser_consume
        procedure :: is_at_end => parser_is_at_end
        procedure :: match => parser_match
        procedure :: expect => parser_expect
        procedure :: error => parser_add_error
        procedure :: error_at_token => parser_add_error_at_token
        procedure :: has_errors => parser_has_errors
        procedure :: get_error_messages => parser_get_error_messages
        procedure :: absorb_errors => parser_absorb_errors

        procedure :: cleanup => parser_cleanup
        procedure :: get_token_at_index => parser_get_token_at_index
        procedure :: get_token_count => parser_get_token_count

        ! Assignment operator
        procedure :: assign => parser_state_assign
        generic :: assignment(=) => assign
    end type parser_state_t

    ! Public constructors
    public :: create_parser_state

contains

    ! Create parser state from tokens (view into caller-owned buffer)
    function create_parser_state(tokens, diagnostic_sink) result(state)
        type(token_t), target, intent(in) :: tokens(:)
        type(error_collection_t), target, intent(inout), optional :: diagnostic_sink
        type(parser_state_t) :: state

        if (size(tokens) > 0) then
            state%tokens => tokens
        else
            nullify (state%tokens)
        end if
        state%current_token = 1
        state%owns_tokens = .false.
        state%generation = 1
        if (present(diagnostic_sink)) state%diagnostic_sink => diagnostic_sink
    end function create_parser_state

    ! Peek at current token without consuming it
    function parser_peek(this) result(current_token)
        class(parser_state_t), intent(in) :: this
        type(token_t) :: current_token

        if (associated(this%tokens) .and. this%current_token >= 1 .and. &
            this%current_token <= size(this%tokens)) then
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
            call this%error_at_token(msg, current)
        end if
    end function parser_expect

    ! Add error with current token context
    subroutine parser_add_error(this, message, suggestion)
        class(parser_state_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: suggestion
        type(token_t) :: current

        current = this%peek()
        call this%error_at_token(message, current, suggestion)
    end subroutine parser_add_error

    subroutine parser_add_error_at_token(this, message, token, suggestion)
        class(parser_state_t), intent(inout) :: this
        character(len=*), intent(in) :: message
        type(token_t), intent(in) :: token
        character(len=*), intent(in), optional :: suggestion

        call this%errors%add_error_with_token(message, token, suggestion=suggestion)
        if (associated(this%diagnostic_sink)) then
            call this%diagnostic_sink%add_error_with_token(message, token, &
                suggestion=suggestion)
        end if
    end subroutine parser_add_error_at_token

    ! Take over the errors a nested parser collected. Sub-parsers built from a
    ! statement token slice are discarded once the statement is parsed, so
    ! without this their diagnostics would never reach the caller that reports
    ! parse failures.
    subroutine parser_absorb_errors(this, other)
        class(parser_state_t), intent(inout) :: this
        class(parser_state_t), intent(in) :: other
        integer :: i

        if (.not. other%errors%has_errors()) return
        if (.not. allocated(other%errors%errors)) return

        do i = 1, other%errors%count
            associate (record => other%errors%errors(i))
                if (allocated(record%suggestion)) then
                    call this%errors%add_error_with_context(record%message, &
                        record%context, severity=record%severity, &
                        suggestion=record%suggestion)
                else
                    call this%errors%add_error_with_context(record%message, &
                        record%context, severity=record%severity)
                end if
            end associate
        end do
    end subroutine parser_absorb_errors

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

    ! Clean up parser state and advance generation
    subroutine parser_cleanup(this)
        class(parser_state_t), intent(inout) :: this

        ! Advance generation to invalidate references
        this%generation = this%generation + 1

        ! Clear tokens
        if (associated(this%tokens)) then
            if (this%owns_tokens) then
                deallocate (this%tokens)
            else
                nullify (this%tokens)
            end if
        end if
        this%owns_tokens = .false.
        nullify (this%diagnostic_sink)

        ! Reset position
        this%current_token = 1
    end subroutine parser_cleanup

    ! Get token at specific index
    function parser_get_token_at_index(this, index) result(token)
        class(parser_state_t), intent(in) :: this
        integer, intent(in) :: index
        type(token_t) :: token

        if (associated(this%tokens) .and. index >= 1 .and. &
            index <= size(this%tokens)) then
            token = this%tokens(index)
        else
            ! Return EOF token for out-of-bounds access
            token%kind = TK_EOF
            token%text = ""
            token%line = 1
            token%column = 1
        end if
    end function parser_get_token_at_index

    ! Get total token count
    function parser_get_token_count(this) result(count)
        class(parser_state_t), intent(in) :: this
        integer :: count

        if (associated(this%tokens)) then
            count = size(this%tokens)
        else
            count = 0
        end if
    end function parser_get_token_count

    ! Assignment operator for parser_state_t.
    ! lhs is intent(out), so its pointer component starts disassociated.
    subroutine parser_state_assign(lhs, rhs)
        class(parser_state_t), intent(out) :: lhs
        type(parser_state_t), intent(in) :: rhs

        ! Copy scalar fields
        lhs%current_token = rhs%current_token
        lhs%generation = rhs%generation
        lhs%errors = rhs%errors
        if (associated(rhs%diagnostic_sink)) then
            lhs%diagnostic_sink => rhs%diagnostic_sink
        else
            nullify (lhs%diagnostic_sink)
        end if

        if (associated(rhs%tokens)) then
            if (rhs%owns_tokens) then
                ! Deep copy: rhs owns its allocation, lhs must own its own
                allocate (lhs%tokens(size(rhs%tokens)))
                lhs%tokens = rhs%tokens
                lhs%owns_tokens = .true.
            else
                ! Safe to alias: view into caller-owned tokens
                lhs%tokens => rhs%tokens
                lhs%owns_tokens = .false.
            end if
        else
            nullify (lhs%tokens)
            lhs%owns_tokens = .false.
        end if
    end subroutine parser_state_assign

end module parser_state_module
