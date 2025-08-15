program test_debug_boundary
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=:), allocatable :: test_source, output, error_msg
    type(format_options_t) :: options
    integer :: i
    
    ! Test the failing case
    test_source = "! Header comment" // new_line('a') // &
                  "! Second line" // new_line('a') // &
                  "program test" // new_line('a') // &
                  "! Body comment" // new_line('a') // &
                  "x = 1" // new_line('a') // &
                  "end program"
    
    print *, "=== BOUNDARY DEBUG (using same API as real test) ==="
    print *, "Input:"
    print *, trim(test_source)
    print *, ""
    
    ! First debug the direct parsing to understand token boundaries
    block
        use fortfront, only: lex_source, parse_tokens, emit_fortran
        use lexer_core, only: token_t, TK_COMMENT, token_type_name
        use ast_core, only: ast_arena_t, create_ast_arena
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index, j
        character(len=:), allocatable :: debug_error
        
        print *, "=== DEBUG: Direct parsing ==="
        call lex_source(test_source, tokens, debug_error)
        print *, "Total tokens:", size(tokens)
        do j = 1, min(5, size(tokens))
            print *, j, ": ", trim(token_type_name(tokens(j)%kind)), " = '", trim(tokens(j)%text), "'"
        end do
        print *, "..."
        print *, ""
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, debug_error)
        print *, "Direct parse result - prog_index:", prog_index
        call emit_fortran(arena, prog_index, output)
        print *, "Direct parse output:"
        print *, trim(output)
        print *, ""
    end block
    
    ! Now use the transform API
    call transform_lazy_fortran_string_with_format(test_source, output, error_msg, options)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "ERROR: ", trim(error_msg)
        stop 1
    end if
    
    print *, "Generated output:"
    print *, trim(output)
    
    ! Check for comments
    if (index(output, "! Header comment") > 0) then
        print *, "✓ Header comment preserved"
    else
        print *, "✗ Header comment missing"
    end if
    
    if (index(output, "! Body comment") > 0) then
        print *, "✓ Body comment preserved"  
    else
        print *, "✗ Body comment missing"
    end if
    
end program test_debug_boundary