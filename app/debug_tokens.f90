program debug_tokens
    use frontend
    use lexer_core, only: token_t, TK_EOF, TK_NEWLINE
    implicit none
    
    character(len=:), allocatable :: input
    type(token_t), allocatable :: tokens(:)
    integer :: i
    
    input = &
        "type :: a"//new_line('A')// &
        "    integer :: t"//new_line('A')// &
        "end type :: a"//new_line('A')// &
        ""//new_line('A')// &
        "program pro"//new_line('A')// &
        "    type(a) :: testy"//new_line('A')// &
        "    testy%t = 3"//new_line('A')// &
        "end program pro"
    
    print *, "Input:"
    print *, input
    print *, ""
    
    ! Use internal lexer interface
    block
        use frontend_core, only: lex_source
        character(len=:), allocatable :: error_msg
        call lex_source(input, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            print *, "Lexer error:", error_msg
            stop 1
        end if
    end block
    
    print *, "Tokens:"
    do i = 1, size(tokens)
        if (tokens(i)%kind == TK_EOF) then
            print '(I3, A)', i, ": [EOF]"
        else if (tokens(i)%kind == TK_NEWLINE) then
            print '(I3, A)', i, ": [NEWLINE]"
        else
            print '(I3, A, A, A, I0, A, I0)', i, ": '", trim(tokens(i)%text), &
                "' (line ", tokens(i)%line, ", col ", tokens(i)%column
        end if
    end do
    
end program debug_tokens