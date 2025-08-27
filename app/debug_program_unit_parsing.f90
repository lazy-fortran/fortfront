program debug_program_unit_parsing
    use frontend_parsing
    use lexer_core
    use ast_arena_modern
    implicit none
    
    character(len=:), allocatable :: test_code
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: unit_index, i
    logical :: has_explicit_program
    
    ! Test with just the type definition
    test_code = &
        'type :: person_t' // new_line('a') // &
        '    character(len=20) :: name' // new_line('a') // &
        'end type person_t'
    
    print *, "=== Debug Program Unit Parsing ==="
    print *, "Test code:"
    print *, test_code
    print *, ""
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    
    print *, "Tokens:"
    do i = 1, size(tokens)
        print *, i, ": '", trim(tokens(i)%text), "' (", tokens(i)%kind, ")"
    end do
    print *, ""
    
    ! Initialize arena
    arena = create_ast_arena(1000)
    
    ! Try to parse this as a program unit
    has_explicit_program = .false.
    print *, "Calling parse_program_unit..."
    
    unit_index = parse_program_unit(tokens, arena, has_explicit_program)
    
    print *, "parse_program_unit returned: ", unit_index
    print *, "Arena now contains: ", arena%size, " nodes"
    
end program debug_program_unit_parsing