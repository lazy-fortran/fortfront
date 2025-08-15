program debug_comment_pipeline
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t, &
                         lex_source, parse_tokens, emit_fortran
    use lexer_core, only: token_t, TK_COMMENT
    use ast_core, only: ast_arena_t, create_ast_arena
    implicit none
    
    character(len=:), allocatable :: test_source, output, error_msg
    type(format_options_t) :: options
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: root_index, i, comment_tokens
    
    ! Test the exact failing case
    test_source = "! Header comment" // new_line('a') // &
                  "! Second line" // new_line('a') // &
                  "program test" // new_line('a') // &
                  "! Body comment" // new_line('a') // &
                  "x = 1" // new_line('a') // &
                  "end program"
    
    print *, "=== COMMENT PIPELINE DIAGNOSTIC ==="
    print *, ""
    print *, "Input source:"
    print *, trim(test_source)
    print *, ""
    
    ! Step 1: Test lexer
    print *, "Step 1: LEXER"
    call lex_source(test_source, tokens, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "LEX ERROR: ", trim(error_msg)
        stop 1
    end if
    
    comment_tokens = 0
    do i = 1, size(tokens)
        if (tokens(i)%kind == TK_COMMENT) then
            comment_tokens = comment_tokens + 1
        end if
    end do
    print *, "Comment tokens:", comment_tokens, "/3 expected"
    
    ! Step 2: Test parser
    print *, ""
    print *, "Step 2: PARSER"
    arena = create_ast_arena()
    call parse_tokens(tokens, arena, root_index, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "PARSE ERROR: ", trim(error_msg)
        stop 1
    end if
    print *, "Parse successful, root_index:", root_index
    
    ! Step 3: Test codegen
    print *, ""
    print *, "Step 3: CODEGEN"
    call emit_fortran(arena, root_index, output)
    
    print *, "Generated output:"
    print *, trim(output)
    print *, ""
    
    ! Check results
    if (index(output, "! Header comment") > 0) then
        print *, "✓ Header comment preserved"
    else
        print *, "✗ Header comment MISSING in codegen"
    end if
    
    if (index(output, "! Body comment") > 0) then
        print *, "✓ Body comment preserved"
    else
        print *, "✗ Body comment MISSING in codegen"
    end if
    
    ! Compare with full pipeline
    print *, ""
    print *, "Step 4: FULL PIPELINE COMPARISON"
    call transform_lazy_fortran_string_with_format(test_source, output, error_msg, options)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "FULL PIPELINE ERROR: ", trim(error_msg)
        stop 1
    end if
    
    print *, "Full pipeline output:"
    print *, trim(output)
    
end program debug_comment_pipeline