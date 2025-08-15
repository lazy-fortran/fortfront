program test_intent_tokens
    use frontend
    use lexer_core, only: token_t, TK_IDENTIFIER, TK_KEYWORD
    implicit none

    character(len=*), parameter :: source = "real, intent(in) :: data"
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    integer :: i

    print *, "Analyzing tokens for:"
    print *, source
    print *, ""
    
    call lex_source(source, tokens, error_msg)
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "Tokenization error: ", error_msg
            stop 1
        end if
    end if
    
    do i = 1, size(tokens)
        print *, "Token ", i, ": '", tokens(i)%text, "' kind=", tokens(i)%kind
        if (tokens(i)%text == "intent") then
            if (tokens(i)%kind == TK_IDENTIFIER) then
                print *, "  -> intent is TK_IDENTIFIER"
            else if (tokens(i)%kind == TK_KEYWORD) then
                print *, "  -> intent is TK_KEYWORD"
            else
                print *, "  -> intent is OTHER kind: ", tokens(i)%kind
            end if
        end if
    end do
    
end program test_intent_tokens