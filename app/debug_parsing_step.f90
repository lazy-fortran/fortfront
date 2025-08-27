program debug_parsing_step
    use frontend_parsing, only: parse_tokens
    use mixed_construct_detector, only: detect_mixed_constructs, mixed_construct_result_t
    use lexer_core
    use ast_arena_modern
    implicit none
    
    character(len=*), parameter :: test_code = &
        'type :: person_t' // new_line('a') // &
        '    integer :: age' // new_line('a') // &
        'end type' // new_line('a') // &
        'program main' // new_line('a') // &
        '    print *, "hello"' // new_line('a') // &
        'end program'
    
    type(token_t), allocatable :: tokens(:)
    type(mixed_construct_result_t) :: mixed_result
    type(ast_arena_t) :: arena
    integer :: prog_index
    character(len=500) :: error_msg
    print *, "Step-by-step parsing debug for Issue #517"
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    print *, "Step 1: Tokenized, got ", size(tokens), " tokens"
    
    ! Detect mixed constructs
    call detect_mixed_constructs(tokens, mixed_result)
    print *, "Step 2: Mixed constructs detected:", mixed_result%has_mixed_constructs
    
    ! Initialize arena
    arena = create_ast_arena(100)
    print *, "Step 3: Arena initialized"
    
    ! This is where it might hang - let's see if we get here
    print *, "Step 4: About to call parse_tokens..."
    call parse_tokens(tokens, arena, prog_index, error_msg)
    
    print *, "Step 5: parse_tokens completed"
    print *, "Result: prog_index=", prog_index, ", error=", trim(error_msg)
    
end program debug_parsing_step