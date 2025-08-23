program test_compiler_arena_simple
    ! Simple test for compiler_arena integration
    
    use compiler_arena
    use ast_factory, only: push_literal
    use ast_base, only: LITERAL_STRING
    implicit none
    
    type(compiler_arena_t) :: arena
    integer :: node_index
    
    print *, "=== Simple Compiler Arena Test ==="
    
    ! Test 1: Create arena
    print *, "Creating compiler arena..."
    arena = create_compiler_arena()
    
    if (arena%is_initialized) then
        print *, "  PASS: Arena created successfully"
    else
        print *, "  FAIL: Arena not initialized"
        stop 1
    end if
    
    ! Test 2: Use AST arena
    print *, "Testing AST arena access..."
    node_index = push_literal(arena%ast, "Hello World", LITERAL_STRING, 1, 1)
    
    if (node_index > 0) then
        print *, "  PASS: AST node created, index =", node_index
    else
        print *, "  FAIL: Failed to create AST node"
        stop 1
    end if
    
    ! Test 3: Generation tracking
    print *, "Testing generation tracking..."
    print *, "  Current generation:", arena%generation
    arena%generation = arena%generation + 1
    print *, "  After increment:", arena%generation
    
    if (arena%generation == 2) then
        print *, "  PASS: Generation tracking works"
    else
        print *, "  FAIL: Generation tracking broken"
        stop 1
    end if
    
    ! Test 4: Statistics
    print *, "Testing statistics..."
    block
        type(compiler_arena_stats_t) :: stats
        stats = arena%get_stats()
        print *, "  Total memory:", stats%total_memory, "bytes"
        print *, "  AST memory:", stats%ast_memory, "bytes"
        print *, "  Type memory:", stats%types_memory, "bytes"
        
        if (stats%total_memory >= 0) then
            print *, "  PASS: Statistics collected"
        else
            print *, "  FAIL: Statistics broken"
            stop 1
        end if
    end block
    
    ! Test 5: Cleanup
    print *, "Testing cleanup..."
    call destroy_compiler_arena(arena)
    
    if (.not. arena%is_initialized) then
        print *, "  PASS: Arena destroyed"
    else
        print *, "  FAIL: Arena not destroyed properly"
        stop 1
    end if
    
    print *, ""
    print *, "All simple tests passed!"
    
end program test_compiler_arena_simple