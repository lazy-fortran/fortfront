program test_arena_stub_methods
    use ast_arena
    use ast_base
    use ast_nodes_core
    implicit none
    
    type(ast_arena_t) :: arena, arena_copy
    type(program_node) :: prog
    type(assignment_node) :: assign
    type(identifier_node) :: id
    type(literal_node) :: lit
    integer :: prog_idx, assign_idx, id_idx, lit_idx
    class(ast_node), allocatable :: current_node, parent_node
    integer, allocatable :: child_indices(:)
    integer :: depth
    type(ast_arena_stats_t) :: stats
    logical :: test_passed
    
    print *, "=== AST Arena Stub Methods Test ==="
    
    test_passed = .true.
    
    ! Create arena with test data
    arena = create_ast_arena()
    
    ! Create literal node: 42
    lit%value = "42"
    lit%literal_type = "integer"
    call arena%push(lit, "literal")
    if (arena%current_index <= 0) then
        print *, "ERROR: Failed to push literal to arena"
        stop 1
    end if
    lit_idx = arena%size
    
    ! Create identifier node: x
    id%name = "x"
    call arena%push(id, "identifier")
    if (arena%current_index <= 0) then
        print *, "ERROR: Failed to push identifier to arena"
        stop 1
    end if
    id_idx = arena%size
    
    ! Create assignment node: x = 42 (with parent relationship)
    assign%target_index = id_idx
    assign%value_index = lit_idx
    assign%operator = "="
    call arena%push(assign, "assignment", lit_idx)  ! Set literal as parent for testing
    if (arena%current_index <= 0) then
        print *, "ERROR: Failed to push assignment to arena"
        stop 1
    end if
    assign_idx = arena%size
    
    ! Create program node (root)
    prog%name = "test_program"
    allocate(prog%body_indices(1))
    prog%body_indices(1) = assign_idx
    call arena%push(prog, "program")
    if (arena%current_index <= 0) then
        print *, "ERROR: Failed to push program to arena"
        stop 1
    end if
    prog_idx = arena%size
    
    ! Test get_depth
    print *, "Testing get_depth..."
    depth = arena%get_depth(prog_idx)
    if (depth == 0) then
        print *, "  ✓ Program node has depth:", depth
    else
        print *, "  ✗ Expected depth 0, got:", depth
        test_passed = .false.
    end if
    
    depth = arena%get_depth(assign_idx)
    if (depth == 1) then
        print *, "  ✓ Assignment node has depth:", depth
    else
        print *, "  ✗ Expected depth 1, got:", depth
        test_passed = .false.
    end if
    
    ! Test current (set current_index manually)
    print *, "Testing current..."
    arena%current_index = lit_idx
    current_node = arena%current()
    if (allocated(current_node)) then
        print *, "  ✓ Current node retrieved successfully"
        deallocate(current_node)
    else
        print *, "  ✗ Failed to get current node"
        test_passed = .false.
    end if
    
    ! Test get_parent
    print *, "Testing get_parent..."
    parent_node = arena%get_parent(assign_idx)
    if (allocated(parent_node)) then
        print *, "  ✓ Parent node retrieved successfully"
        deallocate(parent_node)
    else
        print *, "  ✗ Failed to get parent node"
        test_passed = .false.
    end if
    
    ! Test get_children  
    print *, "Testing get_children..."
    child_indices = arena%get_children(lit_idx)
    if (allocated(child_indices)) then
        print *, "  ✓ Children indices retrieved, count:", size(child_indices)
        deallocate(child_indices)
    else
        print *, "  ✗ Failed to get children indices"
        test_passed = .false.
    end if
    
    ! Test traverse_depth
    print *, "Testing traverse_depth..."
    call arena%traverse_depth(0, arena)  ! Visit depth 0 nodes
    print *, "  ✓ Traverse depth completed (simplified implementation)"
    
    ! Test shrink_arena (should do nothing since arena is small)
    print *, "Testing shrink_arena..."
    stats = arena%get_stats()
    call arena%shrink_arena()
    if (arena%capacity <= stats%capacity) then
        print *, "  ✓ Arena shrink completed, capacity:", arena%capacity
    else
        print *, "  ✗ Arena capacity unexpectedly increased"
        test_passed = .false.
    end if
    
    ! Test deep_copy
    print *, "Testing deep_copy..."
    arena_copy = arena%deep_copy()
    if (arena_copy%size == arena%size) then
        print *, "  ✓ Arena deep copy successful, size:", arena_copy%size
    else
        print *, "  ✗ Arena deep copy failed, expected size:", arena%size, "got:", arena_copy%size
        test_passed = .false.
    end if
    
    ! Test copied arena functionality
    depth = arena_copy%get_depth(prog_idx)
    if (depth == 0) then
        print *, "  ✓ Copied arena maintains depth information"
    else
        print *, "  ✗ Copied arena lost depth information"
        test_passed = .false.
    end if
    
    ! Final result
    if (test_passed) then
        print *, "All arena stub method tests passed!"
    else
        print *, "Some arena stub method tests failed!"
        stop 1
    end if
    
end program test_arena_stub_methods