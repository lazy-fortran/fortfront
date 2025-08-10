program test_issue_199_verification
    ! Verification test for Issue #199: Undefined Variable Detection
    ! 
    ! Issue description: is_identifier_defined_direct incorrectly returns true 
    ! for undefined variables when they appear in assignments.
    ! 
    ! This test verifies the function correctly identifies:
    ! 1. Defined variables (should return true)
    ! 2. Undefined variables (should return false)
    ! 3. Builtin functions (should return true)
    ! 4. Non-existent identifiers (should return false)
    use fortfront
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Issue #199 Verification ==="
    print *, "Testing is_identifier_defined_direct undefined variable detection"
    print *, ""
    
    all_tests_passed = .true.
    
    ! Test core functionality with full semantic analysis pipeline
    if (.not. test_full_semantic_pipeline()) all_tests_passed = .false.
    
    ! Test edge cases with manual semantic context
    if (.not. test_edge_cases()) all_tests_passed = .false.
    
    ! Test builtin vs undefined distinction
    if (.not. test_builtin_vs_undefined()) all_tests_passed = .false.
    
    print *, ""
    if (all_tests_passed) then
        print *, "✅ Issue #199 verification PASSED"
        print *, "   is_identifier_defined_direct works correctly"
    else
        print *, "❌ Issue #199 verification FAILED"
        stop 1
    end if
    
contains
    
    logical function test_full_semantic_pipeline()
        character(len=*), parameter :: code = &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: defined_var" // new_line('a') // &
            "    defined_var = undefined_var" // new_line('a') // &
            "end program test"
        
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        integer :: prog_index
        logical :: defined_result, undefined_result
        
        print *, "Test 1: Full semantic pipeline (exact issue scenario)"
        
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        call lex_source(code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "  ✗ Lex error:", error_msg
            test_full_semantic_pipeline = .false.
            return
        end if
        
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "  ✗ Parse error:", error_msg
            test_full_semantic_pipeline = .false.
            return
        end if
        
        call analyze_program(ctx, arena, prog_index)
        
        defined_result = is_identifier_defined_direct(arena, ctx, "defined_var")
        undefined_result = is_identifier_defined_direct(arena, ctx, "undefined_var")
        
        print *, "  defined_var (should be true):", defined_result
        print *, "  undefined_var (should be false):", undefined_result
        
        test_full_semantic_pipeline = defined_result .and. .not. undefined_result
        
        if (test_full_semantic_pipeline) then
            print *, "  ✓ Full pipeline test passed"
        else
            print *, "  ✗ Full pipeline test failed"
        end if
    end function test_full_semantic_pipeline
    
    logical function test_edge_cases()
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        logical :: empty_result, long_result, special_result
        
        print *, ""
        print *, "Test 2: Edge cases"
        
        arena = create_ast_arena()
        ctx = create_semantic_context()
        call ctx%scopes%enter_block()
        
        ! Test edge cases that might cause issues
        empty_result = is_identifier_defined_direct(arena, ctx, "")
        long_result = is_identifier_defined_direct(arena, ctx, &
            "very_long_identifier_name_that_definitely_should_not_exist_anywhere")
        special_result = is_identifier_defined_direct(arena, ctx, "123invalid")
        
        print *, "  empty string:", empty_result
        print *, "  very long name:", long_result  
        print *, "  invalid identifier:", special_result
        
        test_edge_cases = .not. empty_result .and. .not. long_result .and. .not. special_result
        
        if (test_edge_cases) then
            print *, "  ✓ Edge cases handled correctly"
        else
            print *, "  ✗ Edge cases failed"
        end if
    end function test_edge_cases
    
    logical function test_builtin_vs_undefined()
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        logical :: builtin_result, undefined_result, fake_builtin_result
        
        print *, ""
        print *, "Test 3: Builtin vs undefined distinction"
        
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        builtin_result = is_identifier_defined_direct(arena, ctx, "sin")
        undefined_result = is_identifier_defined_direct(arena, ctx, "truly_undefined")
        fake_builtin_result = is_identifier_defined_direct(arena, ctx, "fake_builtin_func")
        
        print *, "  sin (builtin, should be true):", builtin_result
        print *, "  truly_undefined (should be false):", undefined_result
        print *, "  fake_builtin_func (should be false):", fake_builtin_result
        
        test_builtin_vs_undefined = builtin_result .and. .not. undefined_result .and. .not. fake_builtin_result
        
        if (test_builtin_vs_undefined) then
            print *, "  ✓ Builtin vs undefined distinction works correctly"
        else
            print *, "  ✗ Builtin vs undefined distinction failed"
        end if
    end function test_builtin_vs_undefined
    
end program test_issue_199_verification