program test_colon_detection
    use frontend
    use lexer_core, only: token_t, TK_OPERATOR, TK_EOF
    use parser_state_module, only: parser_state_t, create_parser_state
    implicit none

    character(len=*), parameter :: source = "real, intent(in) :: data"
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    type(parser_state_t) :: parser
    logical :: found_colon
    integer :: i

    print *, "Simulating has_double_colon logic for:"
    print *, source
    print *, ""
    
    call lex_source(source, tokens, error_msg)
    parser = create_parser_state(tokens)
    
    ! Print all tokens with positions
    do i = 1, size(tokens)
        print *, "Token ", i, ": '", tokens(i)%text, "' kind=", tokens(i)%kind
    end do
    print *, ""
    
    ! Simulate has_double_colon logic starting from position 1 (real)
    found_colon = .false.
    print *, "Simulating search from current_token=1:"
    do i = 2, min(21, size(tokens))  ! parser%current_token + 1 to +20
        print *, "  Checking token ", i, ": '", tokens(i)%text, "'"
        if (tokens(i)%kind == TK_OPERATOR .and. tokens(i)%text == "::") then
            found_colon = .true.
            print *, "    -> FOUND :: at position ", i
            exit
        else if (tokens(i)%kind == TK_EOF) then
            print *, "    -> EOF, exiting search"
            exit
        else
            print *, "    -> continue searching"
        end if
    end do
    
    if (found_colon) then
        print *, "SUCCESS: Simulated has_double_colon would return true"
    else
        print *, "FAIL: Simulated has_double_colon would return false"
    end if
    
end program test_colon_detection