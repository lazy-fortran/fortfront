program debug_token_14
    use frontend_parsing
    use lexer_core
    use ast_arena_modern
    implicit none
    
    character(len=:), allocatable :: test_code
    type(token_t), allocatable :: tokens(:), single_token(:)
    type(ast_arena_t) :: arena
    integer :: stmt_index
    logical :: has_explicit_program_unit
    
    ! Full test code
    test_code = &
        'type :: person_t' // new_line('a') // &
        '    character(len=20) :: name' // new_line('a') // &
        'end type person_t' // new_line('a') // &
        '' // new_line('a') // &
        'program main' // new_line('a') // &
        '    print *, "hello"' // new_line('a') // &
        'end program main'
    
    print *, "=== Debug Token 14 Issue ==="
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    
    print *, "Token 14 is: '", trim(tokens(14)%text), "'"
    
    ! Create a token array with just token 14 + EOF
    allocate(single_token(2))
    single_token(1) = tokens(14)
    single_token(2)%kind = TK_EOF
    single_token(2)%text = ""
    single_token(2)%line = tokens(14)%line
    single_token(2)%column = tokens(14)%column + len(tokens(14)%text)
    
    ! Initialize arena
    arena = create_ast_arena(1000)
    has_explicit_program_unit = .false.
    
    print *, "Testing parse_program_unit on just token 14..."
    
    ! This simulates what happens when token 14 is processed as a unit
    stmt_index = parse_program_unit(single_token, arena, has_explicit_program_unit)
    
    print *, "parse_program_unit returned stmt_index: ", stmt_index
    print *, "Arena size: ", arena%size
    
    deallocate(single_token)
    
end program debug_token_14