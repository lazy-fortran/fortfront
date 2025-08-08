program test_parse_steps
    use lexer_core
    use frontend
    use ast_core
    use codegen_core
    implicit none
    
    character(len=*), parameter :: source = "arr = (/ 1, 2, 3, 4 /)"
    type(token_t), allocatable :: tokens(:), stmt_tokens(:)
    character(len=:), allocatable :: error_msg, result
    type(ast_arena_t) :: arena
    integer :: prog_index, i
    integer :: stmt_start, stmt_end
    
    ! Tokenize
    call lex_source(source, tokens, error_msg)
    
    print *, "=== ALL TOKENS ==="
    do i = 1, size(tokens)
        print '(A,I2,A,A,A,I2)', &
            "Token ", i, ": '", trim(tokens(i)%text), &
            "' kind=", tokens(i)%kind
    end do
    
    ! Find statement boundary like parse_all_statements does
    call find_statement_boundary(tokens, 1, stmt_start, stmt_end)
    
    print *, ""
    print *, "=== Statement boundary: ", stmt_start, " to ", stmt_end
    
    ! Extract statement tokens
    allocate(stmt_tokens(stmt_end - stmt_start + 2))
    stmt_tokens(1:stmt_end - stmt_start + 1) = tokens(stmt_start:stmt_end)
    ! Add EOF token
    stmt_tokens(stmt_end - stmt_start + 2)%kind = TK_EOF
    stmt_tokens(stmt_end - stmt_start + 2)%text = ""
    stmt_tokens(stmt_end - stmt_start + 2)%line = tokens(stmt_end)%line
    stmt_tokens(stmt_end - stmt_start + 2)%column = tokens(stmt_end)%column + 1
    
    print *, ""
    print *, "=== STATEMENT TOKENS ==="
    do i = 1, size(stmt_tokens)
        print '(A,I2,A,A,A,I2)', &
            "Token ", i, ": '", trim(stmt_tokens(i)%text), &
            "' kind=", stmt_tokens(i)%kind
    end do
    
contains
    
    ! Copy of find_statement_boundary from frontend
    subroutine find_statement_boundary(tokens, start_pos, stmt_start, stmt_end)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: start_pos
        integer, intent(out) :: stmt_start, stmt_end
        integer :: i, paren_depth
        
        stmt_start = start_pos
        stmt_end = start_pos
        paren_depth = 0
        
        i = start_pos
        do while (i <= size(tokens))
            ! Track parentheses depth
            if (tokens(i)%kind == TK_OPERATOR) then
                if (tokens(i)%text == "(") then
                    paren_depth = paren_depth + 1
                else if (tokens(i)%text == ")") then
                    paren_depth = paren_depth - 1
                end if
            end if
            
            if (tokens(i)%kind == TK_EOF) then
                stmt_end = i - 1
                exit
            else if (tokens(i)%kind == TK_COMMENT .and. i > start_pos) then
                stmt_end = i - 1
                exit
            else if (i < size(tokens) .and. tokens(i)%line < tokens(i + 1)%line .and. &
                     paren_depth == 0) then
                stmt_end = i
                exit
            else
                stmt_end = i
                i = i + 1
            end if
        end do
    end subroutine find_statement_boundary
    
end program test_parse_steps