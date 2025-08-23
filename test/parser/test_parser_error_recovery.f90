program test_parser_error_recovery
    use lexer_core, only: tokenize_safe, TK_KEYWORD, TK_OPERATOR
    use parser_state_module, only: create_parser_state
    use parser_result_types, only: recover_to_statement_boundary, recover_to_closing_paren, &
                                   recover_to_next_token, skip_to_synchronization_point
    use error_handling, only: result_t
    implicit none

    print *, "=== Parser Error Recovery Test ==="

    ! Test statement boundary recovery
    call test_recover_to_statement_boundary()
    call test_recover_to_closing_paren()
    call test_recover_to_next_token()
    call test_skip_to_synchronization_point()

    ! Test error scenarios
    call test_recovery_safety_limits()

    print *, ""
    print *, "=== ALL TESTS PASSED ==="
    print *, "Parser error recovery working correctly!"

contains

    subroutine test_recover_to_statement_boundary()
        type(result_t) :: recovery_res
        character(len=*), parameter :: source = "bad syntax here ; integer :: x"
        
        block
            use parser_state_module, only: parser_state_t
            use lexer_core, only: tokenize_result_t
            
            type(tokenize_result_t) :: lex_result
            type(parser_state_t) :: parser
            
            lex_result = tokenize_safe(source)
            if (lex_result%result%is_failure()) then
                print *, "FAIL: Tokenization should succeed for recovery test"
                stop 1
            end if
            
            parser = create_parser_state(lex_result%tokens)
            
            ! Skip past "bad syntax here"
            block
                use lexer_core, only: token_t
                type(token_t) :: dummy_token
                dummy_token = parser%consume()  ! "bad"
                dummy_token = parser%consume()  ! "syntax"
                dummy_token = parser%consume()  ! "here"
            end block
            
            ! Now recover to statement boundary (semicolon)
            recovery_res = recover_to_statement_boundary(parser)
            
            if (recovery_res%is_failure()) then
                print *, "FAIL: Statement boundary recovery should succeed"
                print *, "Error: ", recovery_res%get_message()
                stop 1
            end if
            
            ! Should be positioned at semicolon
            block
                use lexer_core, only: token_t
                type(token_t) :: token
                token = parser%peek()
                if (token%kind /= TK_OPERATOR .or. token%text /= ";") then
                    print *, "FAIL: Should be positioned at semicolon after recovery"
                    stop 1
                end if
            end block
        end block
        
        print *, "PASS: Statement boundary recovery"
    end subroutine test_recover_to_statement_boundary

    subroutine test_recover_to_closing_paren()
        type(result_t) :: recovery_res
        character(len=*), parameter :: source = "( bad syntax here )"
        
        block
            use parser_state_module, only: parser_state_t
            use lexer_core, only: tokenize_result_t
            
            type(tokenize_result_t) :: lex_result
            type(parser_state_t) :: parser
            
            lex_result = tokenize_safe(source)
            if (lex_result%result%is_failure()) then
                print *, "FAIL: Tokenization should succeed for paren recovery test"
                stop 1
            end if
            
            parser = create_parser_state(lex_result%tokens)
            
            ! Skip opening paren and bad tokens
            block
                use lexer_core, only: token_t
                type(token_t) :: dummy_token
                dummy_token = parser%consume()  ! "("
            end block
            
            ! Now recover to closing paren
            recovery_res = recover_to_closing_paren(parser, "(")
            
            if (recovery_res%is_failure()) then
                print *, "FAIL: Closing paren recovery should succeed"
                print *, "Error: ", recovery_res%get_message()
                stop 1
            end if
            
            ! Should be past the closing paren
            if (.not. parser%is_at_end()) then
                print *, "FAIL: Should be at end after paren recovery"
                stop 1
            end if
        end block
        
        print *, "PASS: Closing parenthesis recovery"
    end subroutine test_recover_to_closing_paren

    subroutine test_recover_to_next_token()
        type(result_t) :: recovery_res
        character(len=*), parameter :: source = "bad syntax here integer :: x"
        
        block
            use parser_state_module, only: parser_state_t
            use lexer_core, only: tokenize_result_t, TK_KEYWORD
            
            type(tokenize_result_t) :: lex_result
            type(parser_state_t) :: parser
            
            lex_result = tokenize_safe(source)
            if (lex_result%result%is_failure()) then
                print *, "FAIL: Tokenization should succeed for token recovery test"
                stop 1
            end if
            
            parser = create_parser_state(lex_result%tokens)
            
            ! Skip "bad" token
            block
                use lexer_core, only: token_t
                type(token_t) :: dummy_token
                dummy_token = parser%consume()
            end block
            
            ! Recover to next keyword token
            recovery_res = recover_to_next_token(parser, TK_KEYWORD)
            
            if (recovery_res%is_failure()) then
                print *, "FAIL: Next token recovery should succeed"
                print *, "Error: ", recovery_res%get_message()
                stop 1
            end if
            
            ! Should be positioned at "integer"
            block
                use lexer_core, only: token_t
                type(token_t) :: token
                token = parser%peek()
                if (token%kind /= TK_KEYWORD .or. token%text /= "integer") then
                    print *, "FAIL: Should be positioned at 'integer' keyword"
                    stop 1
                end if
            end block
        end block
        
        print *, "PASS: Next token recovery"
    end subroutine test_recover_to_next_token

    subroutine test_skip_to_synchronization_point()
        type(result_t) :: recovery_res
        character(len=*), parameter :: source = "bad syntax here function test()"
        
        block
            use parser_state_module, only: parser_state_t
            use lexer_core, only: tokenize_result_t
            
            type(tokenize_result_t) :: lex_result
            type(parser_state_t) :: parser
            
            lex_result = tokenize_safe(source)
            if (lex_result%result%is_failure()) then
                print *, "FAIL: Tokenization should succeed for sync point test"
                stop 1
            end if
            
            parser = create_parser_state(lex_result%tokens)
            
            ! Skip "bad" token
            block
                use lexer_core, only: token_t
                type(token_t) :: dummy_token
                dummy_token = parser%consume()
            end block
            
            ! Skip to synchronization point (function keyword)
            recovery_res = skip_to_synchronization_point(parser)
            
            if (recovery_res%is_failure()) then
                print *, "FAIL: Synchronization point recovery should succeed"
                print *, "Error: ", recovery_res%get_message()
                stop 1
            end if
            
            ! Should be positioned at "function"
            block
                use lexer_core, only: token_t
                type(token_t) :: token
                token = parser%peek()
                if (token%kind /= TK_KEYWORD .or. token%text /= "function") then
                    print *, "FAIL: Should be positioned at 'function' keyword"
                    stop 1
                end if
            end block
        end block
        
        print *, "PASS: Synchronization point recovery"
    end subroutine test_skip_to_synchronization_point

    subroutine test_recovery_safety_limits()
        type(result_t) :: recovery_res
        
        ! Test with a very long token stream to trigger safety limits
        block
            use parser_state_module, only: parser_state_t
            use lexer_core, only: token_t, TK_IDENTIFIER, tokenize_result_t
            
            type(parser_state_t) :: parser
            type(token_t), allocatable :: long_tokens(:)
            integer :: i
            
            ! Create a long sequence of identifier tokens (no statement boundaries)
            allocate(long_tokens(150))
            do i = 1, 150
                long_tokens(i)%kind = TK_IDENTIFIER
                long_tokens(i)%text = "token" // char(ichar('a') + mod(i, 26))
                long_tokens(i)%line = 1
                long_tokens(i)%column = i
            end do
            
            parser = create_parser_state(long_tokens)
            
            ! This should trigger the safety limit
            recovery_res = recover_to_statement_boundary(parser)
            
            if (recovery_res%is_success()) then
                print *, "FAIL: Long token sequence should trigger safety limit"
                stop 1
            end if
            
            ! Should get an error about too many tokens skipped
            if (index(recovery_res%get_message(), "too many tokens") == 0) then
                print *, "FAIL: Should get 'too many tokens' error message"
                stop 1
            end if
        end block
        
        print *, "PASS: Recovery safety limits working"
    end subroutine test_recovery_safety_limits

end program test_parser_error_recovery
