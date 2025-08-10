program test_issue_199_exact_reproduction
    ! Test for issue #199: Exact reproduction attempt based on issue description
    ! This attempts to replicate the exact scenario described in the issue
    use fortfront
    implicit none
    
    character(len=*), parameter :: code = &
        "program test" // new_line('a') // &
        "    implicit none" // new_line('a') // &
        "    integer :: x" // new_line('a') // &
        "    x = undefined_var" // new_line('a') // &
        "end program test"
    
    print *, "=== Issue #199: Exact Reproduction Test ==="
    print *, ""
    print *, "Code under test:"
    print *, code
    print *, ""
    
    ! Test the exact scenario from the issue description
    call test_exact_scenario()
    
    ! Test with various semantic contexts to find the problem
    call test_different_contexts()
    
contains
    
    subroutine test_exact_scenario()
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: sem_ctx
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_msg
        integer :: prog_index
        logical :: x_result, undefined_result
        
        print *, "Test 1: Exact scenario from issue description"
        
        ! Follow the exact steps from the issue
        arena = create_ast_arena()
        
        call lex_source(code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "  ✗ Lexing error:", error_msg
            return
        end if
        
        call parse_tokens(tokens, arena, prog_index, error_msg)  
        if (error_msg /= "") then
            print *, "  ✗ Parsing error:", error_msg
            return
        end if
        
        ! Create a fresh semantic context and run analysis manually
        sem_ctx = create_semantic_context()
        call analyze_program(sem_ctx, arena, prog_index)
        
        ! Now test the identifiers
        x_result = is_identifier_defined_direct(arena, sem_ctx, "x")
        undefined_result = is_identifier_defined_direct(arena, sem_ctx, "undefined_var")
        
        print *, "  Results:"
        print *, "    is_identifier_defined_direct(arena, sem_ctx, 'x'):", x_result
        print *, "    is_identifier_defined_direct(arena, sem_ctx, 'undefined_var'):", undefined_result
        
        if (x_result .and. .not. undefined_result) then
            print *, "  ✓ Expected behavior: x=true, undefined_var=false"
        else
            print *, "  ✗ ISSUE REPRODUCED: Expected x=true, undefined_var=false"
            print *, "      Got x=", x_result, ", undefined_var=", undefined_result
        end if
        
        call debug_context_state(sem_ctx)
    end subroutine test_exact_scenario
    
    subroutine test_different_contexts()
        type(ast_arena_t) :: arena1, arena2
        type(semantic_context_t) :: ctx1, ctx2
        logical :: test1, test2, test3, test4
        
        print *, ""
        print *, "Test 2: Testing with different context scenarios"
        
        ! Scenario 1: Empty context
        arena1 = create_ast_arena()
        ctx1 = create_semantic_context()
        
        test1 = is_identifier_defined_direct(arena1, ctx1, "undefined_var")
        print *, "  Empty context - undefined_var:", test1
        
        ! Scenario 2: Context with only builtins
        ctx2 = create_semantic_context()
        test2 = is_identifier_defined_direct(arena1, ctx2, "undefined_var")
        print *, "  Fresh context - undefined_var:", test2
        
        ! Scenario 3: Check if builtin detection is too broad
        test3 = is_identifier_defined_direct(arena1, ctx2, "should_not_exist_anywhere")
        print *, "  Fresh context - should_not_exist_anywhere:", test3
        
        ! Scenario 4: Check a name that might be mistaken for builtin
        test4 = is_identifier_defined_direct(arena1, ctx2, "program")
        print *, "  Fresh context - program:", test4
        
        if (test1 .or. test2 .or. test3) then
            print *, "  ✗ ISSUE FOUND: Some undefined variables are being detected as defined!"
        else
            print *, "  ✓ All undefined variables correctly identified as undefined"
        end if
    end subroutine test_different_contexts
    
    subroutine debug_context_state(ctx)
        type(semantic_context_t), intent(inout) :: ctx
        integer :: i, j
        
        print *, ""
        print *, "  Debug info - semantic context state:"
        print *, "    Scope depth:", ctx%scopes%depth
        
        if (ctx%scopes%depth > 0) then
            do i = 1, ctx%scopes%depth
                print *, "    Scope", i, "has", ctx%scopes%scopes(i)%env%count, "symbols:"
                do j = 1, min(ctx%scopes%scopes(i)%env%count, 5)  ! Show max 5 symbols
                    print *, "      -", trim(ctx%scopes%scopes(i)%env%names(j))
                end do
                if (ctx%scopes%scopes(i)%env%count > 5) then
                    print *, "      ... and", ctx%scopes%scopes(i)%env%count - 5, "more"
                end if
            end do
        else
            print *, "    No scopes in context"
        end if
    end subroutine debug_context_state
    
end program test_issue_199_exact_reproduction