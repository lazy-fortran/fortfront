program debug_full_parsing
    use frontend_parsing
    use lexer_core
    use ast_arena_modern
    implicit none
    
    character(len=:), allocatable :: test_code
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    character(len=1000) :: error_msg_parse
    integer :: prog_index
    
    ! Test code: type declaration followed by program - same as failing test
    test_code = &
        'type :: person_t' // new_line('a') // &
        '    character(len=20) :: name' // new_line('a') // &
        'end type person_t' // new_line('a') // &
        '' // new_line('a') // &
        'program main' // new_line('a') // &
        '    print *, "hello"' // new_line('a') // &
        'end program main'
    
    print *, "=== Debug Full Parsing ==="
    print *, "Test code:"
    print *, test_code
    print *, ""
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    
    ! Parse with timeout simulation
    arena = create_ast_arena(1000)
    print *, "About to call parse_tokens..."
    
    ! This is the call that hangs in the real test
    call parse_tokens(tokens, arena, prog_index, error_msg_parse)
    
    print *, "parse_tokens completed!"
    print *, "prog_index: ", prog_index
    print *, "error_msg: '", trim(error_msg_parse), "'"
    print *, "Arena size: ", arena%size
    
end program debug_full_parsing