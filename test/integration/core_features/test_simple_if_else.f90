program test_simple_if_else
    ! Simple test to isolate the if/else issue reported in #623
    use iso_fortran_env, only: error_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_core, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none
    
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    character(len=:), allocatable :: error_msg, code, source
    integer :: prog_index
    
    print *, "=== Testing Simple If/Else Issue #623 ==="
    
    ! Test the exact case from the GitHub issue
    source = 'x = 5' // new_line('A') // &
             'if (x > 3) then' // new_line('A') // &
             '  print *, "big"' // new_line('A') // &
             'else' // new_line('A') // &
             '  print *, "small"' // new_line('A') // &
             'end if'
    
    print *, "INPUT SOURCE:"
    print *, source
    print *, ""
    
    ! Process through the pipeline
    call lex_source(source, tokens, error_msg)
    if (error_msg /= "") then
        print *, "LEXING ERROR: ", error_msg
        stop 1
    end if
    
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (error_msg /= "") then
        print *, "PARSING ERROR: ", error_msg
        stop 1
    end if
    
    call emit_fortran(arena, prog_index, code)
    
    print *, "GENERATED CODE:"
    print *, code
    print *, ""
    
    ! Check for the TODO placeholder that indicates the bug
    if (index(code, 'TODO') > 0) then
        print *, "*** ISSUE #623 CONFIRMED: TODO placeholder found ***"
        print *, "Generated code contains TODO instead of proper if/else"
        stop 1
    else if (index(code, 'else') == 0) then
        print *, "*** ISSUE #623 PARTIALLY CONFIRMED: Missing else clause ***"
        print *, "Else clause is missing from generated code"
        stop 1
    else
        print *, "SUCCESS: If/else code generation working correctly"
    end if
    
end program test_simple_if_else