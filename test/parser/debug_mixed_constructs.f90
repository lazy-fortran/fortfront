program debug_mixed_constructs
    use frontend, only: transform_lazy_fortran_string
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_core, only: ast_arena_t, create_ast_arena
    implicit none
    
    character(:), allocatable :: source, cli_output, error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index, i
    
    print *, "=== DEBUG: Finding the difference between CLI and direct parser ==="
    
    source = "module test_mod" // new_line('a') // &
             "  x = 10" // new_line('a') // &
             "end module" // new_line('a') // &
             "y = x + 5"
    
    print *, "Input:"
    print *, trim(source)
    print *
    
    ! Test 1: CLI path (transform_lazy_fortran_string)
    print *, "=== CLI PATH ==="
    call transform_lazy_fortran_string(source, cli_output, error_msg)
    if (error_msg /= "") then
        print *, "CLI Error:", trim(error_msg)
    else
        print *, "CLI Output:"
        print *, trim(cli_output)
    end if
    print *
    
    ! Test 2: Direct parser path (lex_source + parse_tokens)
    print *, "=== DIRECT PARSER PATH ==="
    call lex_source(source, tokens, error_msg)
    if (error_msg /= "") then
        print *, "Lex Error:", trim(error_msg)
        stop 1
    end if
    
    ! Debug: Show all tokens
    print *, "Tokens:"
    do i = 1, size(tokens)
        print *, i, ":", trim(tokens(i)%text), "(", tokens(i)%kind, ")"
    end do
    print *
    
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, error_msg)
    
    if (error_msg /= "") then
        print *, "Parse Error:", trim(error_msg)
    else
        print *, "Parse Success - Program index:", prog_index
        print *, "Arena size:", arena%size
        
        ! Check what's actually in the arena
        if (prog_index > 0 .and. prog_index <= arena%size) then
            print *, "Root node type: (can't inspect easily)"
            print *, "Arena has", arena%size, "nodes"
        end if
        
        ! Also generate code to see what happens
        print *, "Direct parser code generation:"
        block
            use frontend_core, only: emit_fortran
            character(:), allocatable :: direct_output
            call emit_fortran(arena, prog_index, direct_output)
            print *, trim(direct_output)
        end block
    end if
    
end program debug_mixed_constructs