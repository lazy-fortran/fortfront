program test_double_colon
    use frontend
    use lexer_core, only: token_t
    use parser_state_module, only: parser_state_t, create_parser_state
    implicit none

    character(len=*), parameter :: source = "real, intent(in) :: data"
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    type(parser_state_t) :: parser
    logical :: found_colon

    print *, "Testing has_double_colon for:"
    print *, source
    
    call lex_source(source, tokens, error_msg)
    parser = create_parser_state(tokens)
    
    ! This would need access to has_double_colon which is private
    ! Let's just manually check if :: is in the tokens
    block
        integer :: i
        found_colon = .false.
        do i = 1, size(tokens)
            print *, "Token ", i, ": '", tokens(i)%text, "'"
            if (tokens(i)%text == "::") then
                found_colon = .true.
                print *, "  -> Found :: at position ", i
            end if
        end do
    end block
    
    if (found_colon) then
        print *, "SUCCESS: :: token found in sequence"
    else
        print *, "FAIL: :: token not found"
    end if
    
end program test_double_colon