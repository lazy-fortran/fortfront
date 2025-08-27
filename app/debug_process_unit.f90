program debug_process_unit
    use frontend_parsing
    use lexer_core
    use ast_arena_modern
    implicit none
    
    character(len=:), allocatable :: test_code
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: stmt_index
    logical :: has_explicit_program_unit
    
    ! Test with just the type definition to isolate the issue
    test_code = &
        'type :: person_t' // new_line('a') // &
        '    character(len=20) :: name' // new_line('a') // &
        'end type person_t'
    
    print *, "=== Debug Process Unit ==="
    print *, "Test code:"
    print *, test_code
    print *, ""
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    
    ! Initialize arena
    arena = create_ast_arena(1000)
    has_explicit_program_unit = .false.
    
    ! Test parse_program_unit directly (this is what gets called internally)
    print *, "About to call parse_program_unit..."
    
    ! This simulates what process_program_unit does internally
    stmt_index = parse_program_unit(tokens, arena, has_explicit_program_unit)
    
    print *, "parse_program_unit returned stmt_index: ", stmt_index
    print *, "Arena size: ", arena%size
    
end program debug_process_unit