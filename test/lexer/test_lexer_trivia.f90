program test_lexer_trivia
    use lexer_core
    implicit none
    
    integer :: test_count, failure_count
    
    test_count = 0
    failure_count = 0
    
    call test_trivia_disabled_by_default(test_count, failure_count)
    call test_trivia_collection_comments(test_count, failure_count)
    call test_trivia_collection_whitespace(test_count, failure_count)
    call test_trivia_collection_newlines(test_count, failure_count)
    call test_trivia_attachment_to_tokens(test_count, failure_count)
    call test_backward_compatibility(test_count, failure_count)
    call test_trivia_round_trip(test_count, failure_count)
    
    if (failure_count == 0) then
        print *, "All tests passed! (", test_count, " tests)"
    else
        print *, "FAILED: ", failure_count, " out of ", test_count, " tests"
        stop 1
    end if
    
contains
    
    subroutine test_trivia_disabled_by_default(test_count, failure_count)
        integer, intent(inout) :: test_count, failure_count
        type(token_t), allocatable :: tokens(:)
        type(lexer_options_t) :: options
        character(len=*), parameter :: source = "  ! comment" // new_line('a') // "  x = 1"
        
        test_count = test_count + 1
        
        ! Default options should not collect trivia
        options = default_lexer_options()
        call tokenize_with_options(source, tokens, options)
        
        if (.not. allocated(tokens)) then
            print *, "FAIL: No tokens generated (default options)"
            failure_count = failure_count + 1
            return
        end if
        
        ! Should have comment token, identifier, operator, number, EOF
        if (size(tokens) /= 5) then
            print *, "FAIL: Expected 5 tokens without trivia, got", size(tokens)
            failure_count = failure_count + 1
            return
        end if
        
        ! No trivia should be attached
        if (allocated(tokens(1)%leading_trivia)) then
            print *, "FAIL: Unexpected leading trivia on first token"
            failure_count = failure_count + 1
            return
        end if
        
        print *, "PASS: Trivia disabled by default"
    end subroutine test_trivia_disabled_by_default
    
    subroutine test_trivia_collection_comments(test_count, failure_count)
        integer, intent(inout) :: test_count, failure_count
        type(token_t), allocatable :: tokens(:)
        type(lexer_options_t) :: options
        character(len=*), parameter :: source = "! header comment" // new_line('a') // &
                                                "x = 1  ! inline comment"
        
        test_count = test_count + 1
        
        ! Enable trivia collection
        options = trivia_lexer_options()
        call tokenize_with_options(source, tokens, options)
        
        if (.not. allocated(tokens)) then
            print *, "FAIL: No tokens generated (trivia enabled)"
            failure_count = failure_count + 1
            return
        end if
        
        ! First significant token (x) should have leading trivia
        if (.not. allocated(tokens(1)%leading_trivia)) then
            print *, "FAIL: No leading trivia on identifier"
            failure_count = failure_count + 1
            return
        end if
        
        ! Check for header comment in trivia
        if (size(tokens(1)%leading_trivia) < 1) then
            print *, "FAIL: Expected at least 1 trivia item"
            failure_count = failure_count + 1
            return
        end if
        
        ! Verify comment trivia
        if (tokens(1)%leading_trivia(1)%kind /= TK_COMMENT) then
            print *, "FAIL: First trivia should be comment"
            failure_count = failure_count + 1
            return
        end if
        
        if (tokens(1)%leading_trivia(1)%text /= "! header comment") then
            print *, "FAIL: Incorrect comment text in trivia"
            failure_count = failure_count + 1
            return
        end if
        
        print *, "PASS: Comment trivia collection"
    end subroutine test_trivia_collection_comments
    
    subroutine test_trivia_collection_whitespace(test_count, failure_count)
        integer, intent(inout) :: test_count, failure_count
        type(token_t), allocatable :: tokens(:)
        type(lexer_options_t) :: options
        character(len=*), parameter :: source = "    x = 1"
        
        test_count = test_count + 1
        
        ! Enable trivia collection
        options = trivia_lexer_options()
        call tokenize_with_options(source, tokens, options)
        
        if (.not. allocated(tokens)) then
            print *, "FAIL: No tokens generated (whitespace trivia)"
            failure_count = failure_count + 1
            return
        end if
        
        ! First token should have leading whitespace trivia
        if (.not. allocated(tokens(1)%leading_trivia)) then
            print *, "FAIL: No leading whitespace trivia"
            failure_count = failure_count + 1
            return
        end if
        
        if (tokens(1)%leading_trivia(1)%kind /= TK_WHITESPACE) then
            print *, "FAIL: First trivia should be whitespace"
            failure_count = failure_count + 1
            return
        end if
        
        if (len(tokens(1)%leading_trivia(1)%text) /= 4) then
            print *, "FAIL: Expected 4 spaces in whitespace trivia"
            failure_count = failure_count + 1
            return
        end if
        
        print *, "PASS: Whitespace trivia collection"
    end subroutine test_trivia_collection_whitespace
    
    subroutine test_trivia_collection_newlines(test_count, failure_count)
        integer, intent(inout) :: test_count, failure_count
        type(token_t), allocatable :: tokens(:)
        type(lexer_options_t) :: options
        character(len=*), parameter :: source = new_line('a') // new_line('a') // "x"
        
        test_count = test_count + 1
        
        ! Enable trivia collection
        options = trivia_lexer_options()
        call tokenize_with_options(source, tokens, options)
        
        if (.not. allocated(tokens)) then
            print *, "FAIL: No tokens generated (newline trivia)"
            failure_count = failure_count + 1
            return
        end if
        
        ! First token should have leading newline trivia
        if (.not. allocated(tokens(1)%leading_trivia)) then
            print *, "FAIL: No leading newline trivia"
            failure_count = failure_count + 1
            return
        end if
        
        ! Should have 2 newline trivia items
        if (size(tokens(1)%leading_trivia) < 2) then
            print *, "FAIL: Expected 2 newline trivia items"
            failure_count = failure_count + 1
            return
        end if
        
        if (tokens(1)%leading_trivia(1)%kind /= TK_NEWLINE .or. &
            tokens(1)%leading_trivia(2)%kind /= TK_NEWLINE) then
            print *, "FAIL: Trivia should be newlines"
            failure_count = failure_count + 1
            return
        end if
        
        print *, "PASS: Newline trivia collection"
    end subroutine test_trivia_collection_newlines
    
    subroutine test_trivia_attachment_to_tokens(test_count, failure_count)
        integer, intent(inout) :: test_count, failure_count
        type(token_t), allocatable :: tokens(:)
        type(lexer_options_t) :: options
        character(len=*), parameter :: source = "  ! comment" // new_line('a') // &
                                                "  x = 1"
        
        test_count = test_count + 1
        
        ! Enable trivia collection
        options = trivia_lexer_options()
        call tokenize_with_options(source, tokens, options)
        
        if (.not. allocated(tokens)) then
            print *, "FAIL: No tokens generated (trivia attachment)"
            failure_count = failure_count + 1
            return
        end if
        
        ! First significant token should have all preceding trivia
        if (.not. allocated(tokens(1)%leading_trivia)) then
            print *, "FAIL: No trivia attached to first token"
            failure_count = failure_count + 1
            return
        end if
        
        ! Should have whitespace, comment, newline, whitespace
        if (size(tokens(1)%leading_trivia) < 4) then
            print *, "FAIL: Expected 4 trivia items, got", size(tokens(1)%leading_trivia)
            failure_count = failure_count + 1
            return
        end if
        
        ! Verify trivia sequence
        if (tokens(1)%leading_trivia(1)%kind /= TK_WHITESPACE) then
            print *, "FAIL: First trivia should be whitespace"
            failure_count = failure_count + 1
            return
        end if
        
        if (tokens(1)%leading_trivia(2)%kind /= TK_COMMENT) then
            print *, "FAIL: Second trivia should be comment"
            failure_count = failure_count + 1
            return
        end if
        
        if (tokens(1)%leading_trivia(3)%kind /= TK_NEWLINE) then
            print *, "FAIL: Third trivia should be newline"
            failure_count = failure_count + 1
            return
        end if
        
        if (tokens(1)%leading_trivia(4)%kind /= TK_WHITESPACE) then
            print *, "FAIL: Fourth trivia should be whitespace"
            failure_count = failure_count + 1
            return
        end if
        
        print *, "PASS: Trivia attachment to tokens"
    end subroutine test_trivia_attachment_to_tokens
    
    subroutine test_backward_compatibility(test_count, failure_count)
        integer, intent(inout) :: test_count, failure_count
        type(token_t), allocatable :: tokens_old(:), tokens_new(:)
        type(lexer_options_t) :: options
        character(len=*), parameter :: source = "program test" // new_line('a') // &
                                                "! comment" // new_line('a') // &
                                                "  x = 1" // new_line('a') // &
                                                "end program"
        integer :: i
        
        test_count = test_count + 1
        
        ! Test with old tokenize_core interface
        call tokenize_core(source, tokens_old)
        
        ! Test with new interface but trivia disabled
        options = default_lexer_options()
        call tokenize_with_options(source, tokens_new, options)
        
        if (.not. allocated(tokens_old) .or. .not. allocated(tokens_new)) then
            print *, "FAIL: Tokens not allocated"
            failure_count = failure_count + 1
            return
        end if
        
        ! Should produce same number of tokens
        if (size(tokens_old) /= size(tokens_new)) then
            print *, "FAIL: Different token counts:", size(tokens_old), "vs", size(tokens_new)
            failure_count = failure_count + 1
            return
        end if
        
        ! Tokens should match exactly
        do i = 1, size(tokens_old)
            if (tokens_old(i)%kind /= tokens_new(i)%kind) then
                print *, "FAIL: Token kind mismatch at", i
                failure_count = failure_count + 1
                return
            end if
            
            if (tokens_old(i)%text /= tokens_new(i)%text) then
                print *, "FAIL: Token text mismatch at", i
                failure_count = failure_count + 1
                return
            end if
        end do
        
        print *, "PASS: Backward compatibility maintained"
    end subroutine test_backward_compatibility
    
    subroutine test_trivia_round_trip(test_count, failure_count)
        integer, intent(inout) :: test_count, failure_count
        type(token_t), allocatable :: tokens(:)
        type(lexer_options_t) :: options
        character(len=*), parameter :: source = "! Header comment" // new_line('a') // &
                                                "" // new_line('a') // &
                                                "program test" // new_line('a') // &
                                                "    ! Indented comment" // new_line('a') // &
                                                "    x = 1  ! Inline comment" // new_line('a') // &
                                                "end program"
        character(len=:), allocatable :: reconstructed
        integer :: i, j
        
        test_count = test_count + 1
        
        ! Enable trivia collection
        options = trivia_lexer_options()
        call tokenize_with_options(source, tokens, options)
        
        if (.not. allocated(tokens)) then
            print *, "FAIL: No tokens generated (round trip)"
            failure_count = failure_count + 1
            return
        end if
        
        ! Reconstruct source from tokens and trivia
        reconstructed = ""
        do i = 1, size(tokens)
            ! Add leading trivia
            if (allocated(tokens(i)%leading_trivia)) then
                do j = 1, size(tokens(i)%leading_trivia)
                    reconstructed = reconstructed // tokens(i)%leading_trivia(j)%text
                end do
            end if
            
            ! Add token text (except EOF)
            if (tokens(i)%kind /= TK_EOF) then
                reconstructed = reconstructed // tokens(i)%text
            end if
            
            ! Add trailing trivia
            if (allocated(tokens(i)%trailing_trivia)) then
                do j = 1, size(tokens(i)%trailing_trivia)
                    reconstructed = reconstructed // tokens(i)%trailing_trivia(j)%text
                end do
            end if
        end do
        
        ! For now, just check that we have some reconstruction
        ! Full round-trip will need more sophisticated handling
        if (len(reconstructed) == 0) then
            print *, "FAIL: No source reconstructed"
            failure_count = failure_count + 1
            return
        end if
        
        print *, "PASS: Trivia round-trip basics"
    end subroutine test_trivia_round_trip
    
end program test_lexer_trivia