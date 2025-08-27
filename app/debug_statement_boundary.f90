program debug_statement_boundary
    use frontend_parsing
    use lexer_core
    implicit none
    
    character(len=:), allocatable :: test_code
    type(token_t), allocatable :: tokens(:), single_token(:)
    integer :: stmt_start, stmt_end
    
    ! Full test code
    test_code = &
        'type :: person_t' // new_line('a') // &
        '    character(len=20) :: name' // new_line('a') // &
        'end type person_t' // new_line('a') // &
        '' // new_line('a') // &
        'program main' // new_line('a') // &
        '    print *, "hello"' // new_line('a') // &
        'end program main'
    
    print *, "=== Debug Statement Boundary ==="
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    
    print *, "Token 14 is: '", trim(tokens(14)%text), "'"
    
    ! Create a token array with just token 14 + EOF (simulating what parse_all_statements gets)
    allocate(single_token(2))
    single_token(1) = tokens(14)
    single_token(2)%kind = TK_EOF
    single_token(2)%text = ""
    single_token(2)%line = tokens(14)%line
    single_token(2)%column = tokens(14)%column + len(tokens(14)%text)
    
    print *, "Testing find_statement_boundary on token 14..."
    
    ! This could be the source of infinite loops
    call find_statement_boundary(single_token, 1, stmt_start, stmt_end)
    
    print *, "find_statement_boundary returned:"
    print *, "  stmt_start: ", stmt_start
    print *, "  stmt_end: ", stmt_end
    
    deallocate(single_token)
    
end program debug_statement_boundary