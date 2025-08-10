program test_debug_issue_199
    ! Debug test to understand why variable y is not being detected
    use fortfront
    implicit none
    
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
    integer :: i, j
    
    print *, "=== Debugging Issue #199: Why is 'y' not detected? ==="
    print *, ""
    print *, "Code:"
    print *, code
    print *, ""
    
    arena = create_ast_arena()
    ctx = create_semantic_context()
    
    call lex_source(code, tokens, error_msg)
    call parse_tokens(tokens, arena, prog_index, error_msg)
    call analyze_program(ctx, arena, prog_index)
    
    print *, "After semantic analysis:"
    print *, "Scope depth:", ctx%scopes%depth
    
    do i = 1, ctx%scopes%depth
        print *, "Scope", i, "has", ctx%scopes%scopes(i)%env%count, "symbols:"
        do j = 1, ctx%scopes%scopes(i)%env%count
            print *, "  ", j, ":", trim(ctx%scopes%scopes(i)%env%names(j))
        end do
    end do
    
    print *, ""
    print *, "Testing individual variables:"
    print *, "x:", is_identifier_defined_direct(arena, ctx, "x")
    print *, "y:", is_identifier_defined_direct(arena, ctx, "y")  
    print *, "z:", is_identifier_defined_direct(arena, ctx, "z")
    print *, "undefined_var:", is_identifier_defined_direct(arena, ctx, "undefined_var")
    
end program test_debug_issue_199