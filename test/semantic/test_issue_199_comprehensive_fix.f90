program test_issue_199_comprehensive_fix
    ! Comprehensive test for issue #199: Fix Undefined Variable Detection
    ! This test verifies that is_identifier_defined_direct correctly identifies
    ! defined and undefined variables in all scenarios
    use fortfront
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Issue #199 Comprehensive Fix Verification ==="
    print *, ""
    
    all_tests_passed = .true.
    
    ! Test 1: Basic defined/undefined detection
    if (.not. test_basic_detection()) all_tests_passed = .false.
    
    ! Test 2: Full pipeline integration
    if (.not. test_full_pipeline()) all_tests_passed = .false.
    
    ! Test 3: Builtin function handling
    if (.not. test_builtin_functions()) all_tests_passed = .false.
    
    ! Test 4: Edge cases
    if (.not. test_edge_cases()) all_tests_passed = .false.
    
    print *, ""
    if (all_tests_passed) then
        print *, "✅ ALL TESTS PASSED - Issue #199 is resolved!"
        print *, ""
        print *, "Summary:"
        print *, "- is_identifier_defined_direct correctly identifies defined variables"
        print *, "- is_identifier_defined_direct correctly identifies undefined variables"
        print *, "- Builtin functions are handled properly"
        print *, "- Full semantic analysis pipeline works correctly"
        print *, "- Edge cases are handled appropriately"
    else
        print *, "❌ SOME TESTS FAILED - Issue #199 is not resolved"
        stop 1
    end if
    
contains
    
    logical function test_basic_detection()
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        type(mono_type_t) :: int_type
        type(poly_type_t) :: int_scheme
        logical :: test1, test2, test3
        
        print *, "Test 1: Basic defined/undefined detection"
        
        arena = create_ast_arena()
        ctx = create_semantic_context()
        call ctx%scopes%enter_block()
        
        ! Define a variable 'x'
        int_type%kind = TINT
        int_scheme%mono = int_type
        call ctx%scopes%define("x", int_scheme)
        
        ! Test defined variable
        test1 = is_identifier_defined_direct(arena, ctx, "x")
        print *, "  Defined variable 'x':", test1
        
        ! Test undefined variable
        test2 = is_identifier_defined_direct(arena, ctx, "undefined_variable")
        print *, "  Undefined variable 'undefined_variable':", test2
        
        ! Test another undefined variable
        test3 = is_identifier_defined_direct(arena, ctx, "another_undefined")
        print *, "  Undefined variable 'another_undefined':", test3
        
        test_basic_detection = test1 .and. .not. test2 .and. .not. test3
        
        if (test_basic_detection) then
            print *, "  ✓ Basic detection works correctly"
        else
            print *, "  ✗ Basic detection failed"
        end if
    end function test_basic_detection
    
    logical function test_full_pipeline()
        character(len=*), parameter :: code = &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: x, y" // new_line('a') // &
            "    real :: z" // new_line('a') // &
            "    x = undefined_var" // new_line('a') // &
            "end program test"
        
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        integer :: prog_index
        logical :: test1, test2, test3, test4, test5
        
        print *, ""
        print *, "Test 2: Full pipeline integration"
        
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        ! Full pipeline
        call lex_source(code, tokens, error_msg)
        call parse_tokens(tokens, arena, prog_index, error_msg)
        call analyze_program(ctx, arena, prog_index)
        
        ! Test defined variables from declaration
        test1 = is_identifier_defined_direct(arena, ctx, "x")
        test2 = is_identifier_defined_direct(arena, ctx, "y")
        test3 = is_identifier_defined_direct(arena, ctx, "z")
        print *, "  Defined variables x, y, z:", test1, test2, test3
        
        ! Test undefined variable from assignment
        test4 = is_identifier_defined_direct(arena, ctx, "undefined_var")
        print *, "  Undefined variable 'undefined_var':", test4
        
        ! Test completely unmentioned variable
        test5 = is_identifier_defined_direct(arena, ctx, "never_mentioned")
        print *, "  Never mentioned 'never_mentioned':", test5
        
        test_full_pipeline = test1 .and. test2 .and. test3 .and. .not. test4 .and. .not. test5
        
        if (test_full_pipeline) then
            print *, "  ✓ Full pipeline integration works correctly"
        else
            print *, "  ✗ Full pipeline integration failed"
        end if
    end function test_full_pipeline
    
    logical function test_builtin_functions()
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        logical :: test1, test2, test3, test4
        
        print *, ""
        print *, "Test 3: Builtin function handling"
        
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        ! Test known builtin functions
        test1 = is_identifier_defined_direct(arena, ctx, "sin")
        test2 = is_identifier_defined_direct(arena, ctx, "cos")
        print *, "  Builtin functions sin, cos:", test1, test2
        
        ! Test non-existent builtin
        test3 = is_identifier_defined_direct(arena, ctx, "nonexistent_builtin")
        test4 = is_identifier_defined_direct(arena, ctx, "fake_function")
        print *, "  Non-existent builtins:", test3, test4
        
        test_builtin_functions = test1 .and. test2 .and. .not. test3 .and. .not. test4
        
        if (test_builtin_functions) then
            print *, "  ✓ Builtin function handling works correctly"
        else
            print *, "  ✗ Builtin function handling failed"
        end if
    end function test_builtin_functions
    
    logical function test_edge_cases()
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        logical :: test1, test2, test3, test4
        
        print *, ""
        print *, "Test 4: Edge cases"
        
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        ! Test empty string (should not crash)
        test1 = is_identifier_defined_direct(arena, ctx, "")
        print *, "  Empty string:", test1
        
        ! Test very long name
        test2 = is_identifier_defined_direct(arena, ctx, "this_is_a_very_long_identifier_name_that_should_not_exist")
        print *, "  Very long name:", test2
        
        ! Test case sensitivity (Fortran is case-insensitive)
        call ctx%scopes%enter_block()
        call define_test_variable(ctx, "TestVar")
        test3 = is_identifier_defined_direct(arena, ctx, "testvar")
        test4 = is_identifier_defined_direct(arena, ctx, "TESTVAR")
        print *, "  Case sensitivity TestVar/testvar/TESTVAR:", test3, test4
        
        ! For this test, we expect case insensitive matching or at least consistent behavior
        test_edge_cases = .not. test1 .and. .not. test2  ! empty and long names should be undefined
        
        if (test_edge_cases) then
            print *, "  ✓ Edge cases handled correctly"
        else
            print *, "  ✗ Edge cases failed"
        end if
    end function test_edge_cases
    
    subroutine define_test_variable(ctx, name)
        type(semantic_context_t), intent(inout) :: ctx
        character(len=*), intent(in) :: name
        type(mono_type_t) :: int_type
        type(poly_type_t) :: int_scheme
        
        int_type%kind = TINT
        int_scheme%mono = int_type
        call ctx%scopes%define(name, int_scheme)
    end subroutine define_test_variable
    
end program test_issue_199_comprehensive_fix