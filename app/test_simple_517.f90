program test_simple_517
    use frontend_parsing
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
    type(ast_arena_t) :: arena
    integer :: prog_index
    character(len=500) :: error_msg
    
    print *, "Simple test for Issue #517"
    
    ! Tokenize
    call tokenize_core(test_code, tokens)
    print *, "Tokens: ", size(tokens)
    
    ! Parse
    arena = create_ast_arena(100)
    call parse_tokens(tokens, arena, prog_index, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, "Error: ", trim(error_msg)
        stop 1
    else
        print *, "Success! prog_index=", prog_index, ", arena size=", arena%size
    end if
    
end program test_simple_517