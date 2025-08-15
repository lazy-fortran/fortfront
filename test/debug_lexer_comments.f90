program debug_lexer_comments
    use fortfront, only: lex_source
    use lexer_core, only: token_t, token_type_name, TK_COMMENT
    implicit none
    
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: test_source
    character(len=:), allocatable :: error_msg
    integer :: i, comment_count
    
    ! Test the exact failing case from comment preservation
    test_source = "! Header comment" // new_line('a') // &
                  "! Second line" // new_line('a') // &
                  "program test" // new_line('a') // &
                  "! Body comment" // new_line('a') // &
                  "x = 1" // new_line('a') // &
                  "end program"
    
    print *, "=== LEXER DIAGNOSTIC: Comment Token Analysis ==="
    print *, ""
    print *, "Input source:"
    print *, trim(test_source)
    print *, ""
    
    call lex_source(test_source, tokens, error_msg)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "ERROR: ", trim(error_msg)
        stop 1
    end if
    
    print *, "Total tokens:", size(tokens)
    print *, ""
    
    comment_count = 0
    do i = 1, size(tokens)
        if (tokens(i)%kind == TK_COMMENT) then
            comment_count = comment_count + 1
            print *, "Comment token", comment_count, ": '", trim(tokens(i)%text), "'"
        else
            print *, "Token", i, ": ", trim(token_type_name(tokens(i)%kind)), " = '", trim(tokens(i)%text), "'"
        end if
    end do
    
    print *, ""
    print *, "Total comment tokens found:", comment_count
    
    ! Expected: 3 comment tokens
    if (comment_count >= 3) then
        print *, "✓ LEXER: All expected comments tokenized"
    else
        print *, "✗ LEXER: Missing comment tokens (expected 3, got", comment_count, ")"
    end if
    
end program debug_lexer_comments