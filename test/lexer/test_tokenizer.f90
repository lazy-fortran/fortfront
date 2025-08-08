program test_tokenizer
    use lexer_core
    use frontend, only: lex_source
    implicit none
    
    character(len=*), parameter :: source = "arr = (/ 1, 2, 3, 4 /)"
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    integer :: i
    
    ! Tokenize
    call lex_source(source, tokens, error_msg)
    
    print *, "=== TOKENS for: ", source, " ==="
    do i = 1, size(tokens)
        print '(A,I2,A,A10,A,I2,A,I2,A,I2)', &
            "Token ", i, ": '", tokens(i)%text, &
            "' kind=", tokens(i)%kind, &
            " line=", tokens(i)%line, &
            " col=", tokens(i)%column
    end do
    
end program test_tokenizer