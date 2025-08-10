program test_issue_199_full_pipeline
    ! Test for issue #199: Full pipeline test to reproduce the exact issue
    ! This test runs the complete lex->parse->analyze pipeline and then tests 
    ! is_identifier_defined_direct with the SAME semantic context
    use fortfront
    implicit none
    
    character(len=*), parameter :: test_code = &
        "program test" // new_line('a') // &
        "    implicit none" // new_line('a') // &
        "    integer :: x" // new_line('a') // &
        "    x = undefined_variable" // new_line('a') // &
        "end program test"
    
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg
    integer :: prog_index
    logical :: x_defined, undefined_var_defined
    
    print *, "=== Testing Issue #199: Full Pipeline Test ==="
    print *, ""
    print *, "Test code:"
    print *, test_code
    print *, ""
    
    ! Step 1: Lex the source
    arena = create_ast_arena()
    call lex_source(test_code, tokens, error_msg)
    if (error_msg /= "") then
        print *, "✗ Lexing failed:", error_msg
        stop 1
    end if
    print *, "✓ Lexing completed,", size(tokens), "tokens generated"
    
    ! Step 2: Parse tokens into AST
    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (error_msg /= "") then
        print *, "✗ Parsing failed:", error_msg
        stop 1
    end if
    print *, "✓ Parsing completed, program index:", prog_index
    
    ! Step 3: Analyze semantics - THIS is where the problem likely occurs
    call analyze_semantics(arena, prog_index)
    print *, "✓ Semantic analysis completed"
    
    print *, ""
    print *, "Now we need to test with semantic context that has SAME state as after analysis..."
    print *, "But the issue is that analyze_semantics() creates its own internal context!"
    print *, ""
    
    ! The REAL problem: analyze_semantics creates a local semantic context
    ! that gets destroyed after the call. We need access to that context
    ! to test is_identifier_defined_direct properly.
    
    ! This is why the issue exists - there's no way to get the semantic context
    ! that was used during analysis to test undefined variables!
    
    call test_with_manual_semantic_analysis()
    
contains
    
    subroutine test_with_manual_semantic_analysis()
        type(semantic_context_t) :: ctx
        type(ast_arena_t) :: test_arena
        type(token_t), allocatable :: test_tokens(:)
        character(len=:), allocatable :: test_error_msg
        integer :: test_prog_index
        logical :: test1, test2
        
        print *, "Alternative: Manual semantic analysis with accessible context"
        
        ! Recreate the pipeline but with accessible semantic context
        test_arena = create_ast_arena()
        call lex_source(test_code, test_tokens, test_error_msg)
        call parse_tokens(test_tokens, test_arena, test_prog_index, test_error_msg)
        
        ! Create semantic context
        ctx = create_semantic_context()
        
        ! Manually call analyze_program (the core function)
        call analyze_program(ctx, test_arena, test_prog_index)
        print *, "✓ Manual semantic analysis with accessible context completed"
        
        ! Now test is_identifier_defined_direct with the SAME context
        print *, ""
        print *, "Testing is_identifier_defined_direct with analysis context:"
        
        test1 = is_identifier_defined_direct(test_arena, ctx, "x")
        print *, "  is_identifier_defined_direct(arena, ctx, 'x'):", test1
        
        test2 = is_identifier_defined_direct(test_arena, ctx, "undefined_variable")
        print *, "  is_identifier_defined_direct(arena, ctx, 'undefined_variable'):", test2
        
        print *, ""
        print *, "Scope debugging information:"
        call debug_scope_contents(ctx)
        
        print *, ""
        if (test1 .and. .not. test2) then
            print *, "✅ Manual analysis test PASSED - Issue might be elsewhere"
        else
            print *, "❌ Manual analysis test FAILED - This IS the issue"
            print *, "   Expected: x=T, undefined_variable=F"  
            print *, "   Actual:   x=", test1, ", undefined_variable=", test2
        end if
    end subroutine test_with_manual_semantic_analysis
    
    subroutine debug_scope_contents(ctx)
        type(semantic_context_t), intent(inout) :: ctx
        integer :: i, j
        
        print *, "  Current scope depth:", ctx%scopes%depth
        
        do i = 1, ctx%scopes%depth
            print *, "  Scope level", i, "contains", ctx%scopes%scopes(i)%env%count, "symbols:"
            do j = 1, ctx%scopes%scopes(i)%env%count
                print *, "    -", trim(ctx%scopes%scopes(i)%env%names(j))
            end do
        end do
    end subroutine debug_scope_contents
    
end program test_issue_199_full_pipeline