program test_issue_199_undefined_variable
    ! Test for issue #199: Fix Undefined Variable Detection in is_identifier_defined_direct
    ! The function should correctly identify undefined variables
    use fortfront
    implicit none
    
    type(ast_arena_t) :: arena
    type(semantic_context_t) :: sem_ctx
    logical :: test1, test2, test3, test4
    
    print *, "=== Testing Issue #199: Undefined Variable Detection ==="
    print *, ""
    
    ! Create arena and semantic context
    arena = create_ast_arena()
    sem_ctx = create_semantic_context()
    
    ! Set up a test semantic context with some defined variables
    call setup_test_semantic_context(sem_ctx)
    
    print *, ""
    print *, "Testing is_identifier_defined_direct function:"
    
    ! Test 1: Check defined variable 'x'
    test1 = is_identifier_defined_direct(arena, sem_ctx, "x")
    print *, "  is_identifier_defined_direct(arena, sem_ctx, 'x'):", test1
    if (test1) then
        print *, "  ✓ Correctly identified 'x' as defined"
    else
        print *, "  ✗ FAILED: 'x' should be defined but was not detected"
    end if
    
    ! Test 2: Check undefined variable 'undefined_variable'
    test2 = is_identifier_defined_direct(arena, sem_ctx, "undefined_variable")
    print *, "  is_identifier_defined_direct(arena, sem_ctx, 'undefined_variable'):", test2
    if (.not. test2) then
        print *, "  ✓ Correctly identified 'undefined_variable' as undefined"
    else
        print *, "  ✗ FAILED: 'undefined_variable' should be undefined but was detected as defined"
    end if
    
    ! Test 3: Check some common built-in functions (these should be defined)
    test3 = is_identifier_defined_direct(arena, sem_ctx, "sin")
    print *, "  is_identifier_defined_direct(arena, sem_ctx, 'sin'):", test3
    if (test3) then
        print *, "  ✓ Correctly identified 'sin' as defined (builtin)"
    else
        print *, "  ✗ FAILED: 'sin' should be defined as builtin"
    end if
    
    ! Test 4: Check non-existent builtin
    test4 = is_identifier_defined_direct(arena, sem_ctx, "nonexistent_builtin")
    print *, "  is_identifier_defined_direct(arena, sem_ctx, 'nonexistent_builtin'):", test4
    if (.not. test4) then
        print *, "  ✓ Correctly identified 'nonexistent_builtin' as undefined"
    else
        print *, "  ✗ FAILED: 'nonexistent_builtin' should be undefined"
    end if
    
    print *, ""
    print *, "Scope debugging information:"
    call debug_scope_contents(sem_ctx)
    
    print *, ""
    if (test1 .and. .not. test2 .and. test3 .and. .not. test4) then
        print *, "✅ All tests PASSED - Issue #199 is resolved!"
    else
        print *, "❌ Tests FAILED - Issue #199 still exists"
        print *, "   Expected: x=T, undefined_variable=F, sin=T, nonexistent_builtin=F"
        print *, "   Actual:   x=", test1, ", undefined_variable=", test2, ", sin=", test3, ", nonexistent_builtin=", test4
    end if
    
contains
    
    subroutine setup_test_semantic_context(ctx)
        type(semantic_context_t), intent(inout) :: ctx
        type(mono_type_t) :: int_type
        type(poly_type_t) :: int_scheme
        
        ! Enter global scope
        call ctx%scopes%enter_block()
        
        ! Define variable 'x' as integer
        int_type%kind = TINT
        int_scheme%mono = int_type
        call ctx%scopes%define("x", int_scheme)
        
        print *, "✓ Test semantic context set up with 'x' defined as integer"
    end subroutine setup_test_semantic_context
    
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
    
end program test_issue_199_undefined_variable