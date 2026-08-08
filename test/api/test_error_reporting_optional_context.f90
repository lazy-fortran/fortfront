program test_error_reporting_optional_context
    use error_api, only: error_collection_t, error_context_t, error_record_t, &
        create_error_context, create_error_context_from_token, &
        format_error_message
    use lexer_token_types, only: token_t, TK_IDENTIFIER
    implicit none

    type(token_t) :: token
    type(error_collection_t) :: errors
    type(error_context_t) :: context
    type(error_record_t) :: error
    character(len=:), allocatable :: formatted
    character(len=32) :: source_lines(2)

    token%kind = TK_IDENTIFIER
    token%text = 'associate'
    token%line = 2
    token%column = 3
    source_lines = [character(len=32) :: 'first source line', 'second source line']

    ! Parser diagnostics use this omitted-source-lines path.
    context = create_error_context_from_token(token)
    call require(context%line == 2 .and. context%column == 3, &
        'token location was not preserved without source lines')
    call require(.not. allocated(context%source_line), &
        'omitted source lines unexpectedly produced source context')

    call errors%add_error_with_token('token diagnostic', token)
    call require(errors%count == 1 .and. errors%errors(1)%context%line == 2, &
        'token diagnostic collection path did not preserve location')
    call require(.not. allocated(errors%errors(1)%context%source_line), &
        'token diagnostic collection path invented source context')

    context = create_error_context_from_token(token, source_lines=source_lines)
    call require(allocated(context%source_line) .and. &
        trim(context%source_line) == 'second source line', &
        'valid source-line context was not attached')

    ! Scanner diagnostics can carry source locations beyond the old
    ! fixed-width formatting buffer.
    error%message = 'long location'
    error%context = create_error_context(2592, 12)
    formatted = format_error_message(error)
    call require(index(formatted, 'line 2592, column 12') > 0, &
        'long source location was truncated or rejected')

    print *, 'PASS: optional token error context contract'

contains

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, 'FAIL: ', trim(message)
            error stop 1
        end if
    end subroutine require

end program test_error_reporting_optional_context
