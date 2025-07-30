program test_json_writer_direct
    use json_writer
    use lexer_core
    use ast_core
    use ast_factory
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== JSON Writer Direct Tests ==="
    
    ! Test tokens to JSON string conversion
    call test_tokens_to_string()
    
    ! Test tokens to JSON file (basic functionality)
    call test_tokens_to_file()
    
    ! Test empty tokens array
    call test_empty_tokens()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All JSON writer tests passed!"
        stop 0
    else
        print *, "Some JSON writer tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_tokens_to_string()
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: json_str
        
        call test_start("Tokens to JSON string")
        
        ! Create test tokens
        allocate(tokens(3))
        tokens(1) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="=", line=1, column=3)
        tokens(3) = token_t(kind=TK_NUMBER, text="42", line=1, column=5)
        
        ! Convert to JSON string
        json_str = json_write_tokens_to_string(tokens)
        
        ! Verify result contains expected elements
        if (allocated(json_str) .and. len(json_str) > 0) then
            if (index(json_str, "tokens") > 0 .and. &
                index(json_str, "identifier") > 0 .and. &
                index(json_str, "operator") > 0 .and. &
                index(json_str, "number") > 0) then
                call test_pass()
            else
                call test_fail("JSON string missing expected content")
            end if
        else
            call test_fail("JSON string not generated")
        end if
        
        deallocate(tokens)
    end subroutine test_tokens_to_string
    
    subroutine test_tokens_to_file()
        type(token_t), allocatable :: tokens(:)
        character(len=*), parameter :: test_file = "test_output.json"
        
        call test_start("Tokens to JSON file")
        
        ! Create simple token
        allocate(tokens(1))
        tokens(1) = token_t(kind=TK_KEYWORD, text="program", line=1, column=1)
        
        ! Write to file (test basic functionality)
        call json_write_tokens_to_file(tokens, test_file)
        
        ! If we get here without error, the basic function works
        call test_pass()
        
        deallocate(tokens)
    end subroutine test_tokens_to_file
    
    subroutine test_empty_tokens()
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: json_str
        
        call test_start("Empty tokens array")
        
        ! Create empty tokens array
        allocate(tokens(0))
        
        ! Convert to JSON string
        json_str = json_write_tokens_to_string(tokens)
        
        ! Should generate valid JSON with empty tokens array
        if (allocated(json_str) .and. len(json_str) > 0) then
            if (index(json_str, "tokens") > 0) then
                call test_pass()
            else
                call test_fail("Empty tokens JSON missing 'tokens' key")
            end if
        else
            call test_fail("Failed to generate JSON for empty tokens")
        end if
        
        deallocate(tokens)
    end subroutine test_empty_tokens
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_pass()
        print *, " ... PASSED"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
    end subroutine test_fail
    
end program test_json_writer_direct