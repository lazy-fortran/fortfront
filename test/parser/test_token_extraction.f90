program test_token_extraction
    use fortfront
    use ast_core
    implicit none

    print *, "=== Testing Token Extraction in Function Bodies ==="
    call test_token_extraction_debug()

contains

    subroutine test_token_extraction_debug()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        integer :: i
        
        print *, "Checking how 'go to 100' gets tokenized in function context..."
        
        source = "program test" // new_line('a') // &
                "contains" // new_line('a') // &
                "function test_func()" // new_line('a') // &
                "  go to 100" // new_line('a') // &
                "end function test_func" // new_line('a') // &
                "end program test"
        
        call lex_source(source, tokens, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, "Lexer error:", error_msg
            return
        end if
        
        print *, "Total tokens:", size(tokens)
        print *, "All tokens:"
        do i = 1, size(tokens)
            print *, "  Token", i, ": '", trim(tokens(i)%text), "' kind:", tokens(i)%kind, " line:", tokens(i)%line
        end do
        
        ! Find where "go to 100" statement starts
        do i = 1, size(tokens) - 2
            if (tokens(i)%text == "go" .and. tokens(i+1)%text == "to") then
                print *, "Found 'go to' at token", i, "line", tokens(i)%line
                print *, "  Token", i, ": '", trim(tokens(i)%text), "' kind:", tokens(i)%kind
                print *, "  Token", i+1, ": '", trim(tokens(i+1)%text), "' kind:", tokens(i+1)%kind
                print *, "  Token", i+2, ": '", trim(tokens(i+2)%text), "' kind:", tokens(i+2)%kind
                exit
            end if
        end do
    end subroutine test_token_extraction_debug

end program test_token_extraction