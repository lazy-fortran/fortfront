program debug_derived_type
    use lexer_core
    use parser_state_module
    use parser_declarations
    use ast_arena_modern
    implicit none
    
    character(len=:), allocatable :: test_code
    type(token_t), allocatable :: tokens(:)
    type(parser_state_t) :: parser
    type(ast_arena_t) :: arena
    integer :: type_index
    
    ! Test with type definition
    test_code = &
        'type :: person_t' // new_line('a') // &
        '    character(len=20) :: name' // new_line('a') // &
        'end type person_t'
    
    print *, "=== Debug Derived Type Parser ==="
    print *, "Test code:"
    print *, test_code
    print *, ""
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    
    ! Initialize parser and arena
    parser = create_parser_state(tokens)
    arena = create_ast_arena(1000)
    
    print *, "About to call parse_derived_type_def..."
    
    ! This is what parse_type_unit does internally
    type_index = parse_derived_type_def(parser, arena)
    
    print *, "parse_derived_type_def returned type_index: ", type_index
    print *, "Arena size: ", arena%size
    
end program debug_derived_type