program test_issue_199_fix_verification
    ! Test to verify that Issue #199 is resolved
    ! Issue: is_identifier_defined_direct incorrectly returns true for undefined variables
    ! Expected: Function should return false for truly undefined variables
    use fortfront
    implicit none
    
    print *, "=== Issue #199 Fix Verification ==="
    print *, "Testing is_identifier_defined_direct for undefined variable detection"
    print *, ""
    
    ! Test the exact scenario from the issue description
    call test_issue_scenario()
    
    ! Test various undefined variable cases
    call test_undefined_variables()
    
    print *, ""
    print *, "✅ Issue #199 verification PASSED"
    print *, "   is_identifier_defined_direct correctly identifies undefined variables"
    print *, "   Function returns false for undefined variables as expected"
    
contains
    
    subroutine test_issue_scenario()
        character(len=*), parameter :: code = &
            "program test" // new_line('a') // &
            "    implicit none" // new_line('a') // &
            "    integer :: x" // new_line('a') // &
            "    x = undefined_variable" // new_line('a') // &
            "end program test"
        
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        integer :: prog_index
        logical :: x_result, undefined_result
        
        print *, "Test 1: Issue scenario - assignment to undefined variable"
        print *, "Code: x = undefined_variable"
        
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        call lex_source(code, tokens, error_msg)
        call parse_tokens(tokens, arena, prog_index, error_msg)
        call analyze_program(ctx, arena, prog_index)
        
        x_result = is_identifier_defined_direct(arena, ctx, "x")
        undefined_result = is_identifier_defined_direct(arena, ctx, "undefined_variable")
        
        print *, "  x (defined variable):", x_result
        print *, "  undefined_variable (undefined):", undefined_result
        
        if (x_result .and. .not. undefined_result) then
            print *, "  ✓ Issue scenario works correctly"
        else
            print *, "  ✗ Issue scenario failed - Issue #199 not resolved"
            stop 1
        end if
    end subroutine test_issue_scenario
    
    subroutine test_undefined_variables()
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        logical :: test1, test2, test3, test4, test5
        
        print *, ""
        print *, "Test 2: Various undefined variable cases"
        
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        ! Create minimal context with one defined variable
        call ctx%scopes%enter_block()
        call define_simple_variable(ctx, "defined_var")
        
        ! Test undefined variables that should return false
        test1 = is_identifier_defined_direct(arena, ctx, "undefined_1")
        test2 = is_identifier_defined_direct(arena, ctx, "undefined_2")
        test3 = is_identifier_defined_direct(arena, ctx, "some_random_name")
        test4 = is_identifier_defined_direct(arena, ctx, "nonexistent")
        test5 = is_identifier_defined_direct(arena, ctx, "fake_variable")
        
        print *, "  undefined_1:", test1
        print *, "  undefined_2:", test2
        print *, "  some_random_name:", test3
        print *, "  nonexistent:", test4
        print *, "  fake_variable:", test5
        
        ! Also test the defined variable for comparison
        print *, "  defined_var (should be true):", is_identifier_defined_direct(arena, ctx, "defined_var")
        
        if (.not. test1 .and. .not. test2 .and. .not. test3 .and. .not. test4 .and. .not. test5) then
            print *, "  ✓ All undefined variables correctly return false"
        else
            print *, "  ✗ Some undefined variables incorrectly return true - Issue #199 not resolved"
            stop 1
        end if
    end subroutine test_undefined_variables
    
    subroutine define_simple_variable(ctx, name)
        type(semantic_context_t), intent(inout) :: ctx
        character(len=*), intent(in) :: name
        type(mono_type_t) :: int_type
        type(poly_type_t) :: int_scheme
        
        int_type%kind = TINT
        int_scheme%mono = int_type
        call ctx%scopes%define(name, int_scheme)
    end subroutine define_simple_variable
    
end program test_issue_199_fix_verification