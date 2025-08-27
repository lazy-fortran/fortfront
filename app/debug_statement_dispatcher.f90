program debug_statement_dispatcher
    use frontend_parsing
    use lexer_core
    use parser_dispatcher_module
    use ast_arena_modern
    implicit none
    
    character(len=:), allocatable :: test_code
    type(token_t), allocatable :: tokens(:), type_tokens(:)
    type(ast_arena_t) :: arena
    integer :: stmt_index, i
    
    ! Full test code
    test_code = &
        'type :: person_t' // new_line('a') // &
        '    character(len=20) :: name' // new_line('a') // &
        'end type person_t' // new_line('a') // &
        '' // new_line('a') // &
        'program main' // new_line('a') // &
        '    print *, "hello"' // new_line('a') // &
        'end program main'
    
    print *, "=== Debug Statement Dispatcher on Type Definition ==="
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    
    print *, "Full token array has ", size(tokens), " tokens"
    
    ! Extract just the type definition tokens (1-13 based on boundary detection results)
    ! Plus add EOF token
    allocate(type_tokens(14))
    type_tokens(1:13) = tokens(1:13)
    type_tokens(14)%kind = TK_EOF
    type_tokens(14)%text = ""
    type_tokens(14)%line = tokens(13)%line
    type_tokens(14)%column = tokens(13)%column + len(tokens(13)%text)
    
    print *, "Type definition tokens (1-13):"
    do i = 1, 13
        print *, "  ", i, ": '", trim(type_tokens(i)%text), "'"
    end do
    print *, ""
    
    ! Initialize arena
    arena = create_ast_arena(1000)
    
    print *, "About to call parse_statement_dispatcher on type definition..."
    
    ! This is what the fixed parse_program_unit does
    stmt_index = parse_statement_dispatcher(type_tokens, arena)
    
    print *, "parse_statement_dispatcher returned stmt_index: ", stmt_index
    print *, "Arena size: ", arena%size
    
    deallocate(type_tokens)
    
end program debug_statement_dispatcher