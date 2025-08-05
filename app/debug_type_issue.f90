program debug_type_issue
    use fortfront, only: lex_source, parse_tokens, analyze_semantics, &
                         create_ast_arena, token_t, ast_arena_t
    implicit none
    
    character(len=:), allocatable :: source_code
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    type(ast_arena_t) :: arena
    integer :: prog_index
    
    ! Simple test that should trigger the type mismatch error
    source_code = 'program test_math' // new_line('A') // &
                  '    real :: a = 1.0, b = 2.0' // new_line('A') // &
                  '    real :: result' // new_line('A') // &
                  '    result = sqrt(a**2 + b**2)' // new_line('A') // &
                  'end program test_math'
    
    print *, "DEBUG: Testing type mismatch issue reproduction..."
    print *, "Source code:"
    print *, source_code
    print *, ""
    
    ! Lex the source
    call lex_source(source_code, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "Lexing error:", error_msg
        stop 1
    end if
    print *, "DEBUG: Lexing successful,", size(tokens), "tokens"
    
    ! Parse tokens to AST
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "Parsing error:", error_msg
        stop 1
    end if
    print *, "DEBUG: Parsing successful, arena size:", arena%size
    
    ! This should trigger the type mismatch error
    print *, "DEBUG: Starting semantic analysis..."
    call analyze_semantics(arena, prog_index)
    print *, "DEBUG: Semantic analysis completed successfully"
    
end program debug_type_issue