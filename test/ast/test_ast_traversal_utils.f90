program test_ast_traversal_utils
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_core, only: identifier_node, assignment_node, program_node
    use ast_traversal_utils, only: find_nodes_by_type, get_ancestor_of_type, &
        has_child_of_type, get_children, &
        traverse_ast, traverse_callback
    use test_ast_traversal_utils_helpers, only: traverse_counter_t, count_callback
    implicit none

    type(ast_arena_t) :: arena
    integer :: prog_idx, assign1_idx, assign2_idx, id1_idx, id2_idx, id3_idx

    print *, "=== AST Traversal Utils Tests ==="
    print *

    call test_find_nodes_by_type()
    call test_get_ancestor_of_type()
    call test_has_child_of_type()
    call test_get_children()
    call test_traverse_ast()

    print *
    print *, "All AST traversal utils tests passed!"

contains

    subroutine test_find_nodes_by_type()
        type(program_node) :: prog
        type(assignment_node) :: assign
        type(identifier_node) :: id
        integer, allocatable :: found_indices(:)
        integer :: i

        print *, "Testing find_nodes_by_type..."

        arena = create_ast_arena(16)

        prog%name = "test"
        call arena%push(prog, "program_node", 0)
        prog_idx = arena%compat_size

        id%name = "x"
        call arena%push(id, "identifier_node", prog_idx)
        id1_idx = arena%compat_size

        assign%target_index = 0
        assign%value_index = 0
        assign%operator = "="
        call arena%push(assign, "assignment_node", prog_idx)
        assign1_idx = arena%compat_size

        id%name = "y"
        call arena%push(id, "identifier_node", assign1_idx)
        id2_idx = arena%compat_size

        id%name = "z"
        call arena%push(id, "identifier_node", prog_idx)
        id3_idx = arena%compat_size

        found_indices = find_nodes_by_type(arena, prog_idx, "identifier_node")

        if (size(found_indices) /= 3) then
            print *, "  FAIL: Expected 3 identifier nodes, found", &
                size(found_indices)
            stop 1
        end if

        print *, "  PASS: Found", size(found_indices), "identifier nodes"

        found_indices = find_nodes_by_type(arena, prog_idx, "assignment_node")

        if (size(found_indices) /= 1) then
            print *, "  FAIL: Expected 1 assignment node, found", &
                size(found_indices)
            stop 1
        end if

        print *, "  PASS: Found", size(found_indices), "assignment node"

        found_indices = find_nodes_by_type(arena, prog_idx, "nonexistent")

        if (size(found_indices) /= 0) then
            print *, "  FAIL: Expected 0 nonexistent nodes, found", &
                size(found_indices)
            stop 1
        end if

        print *, "  PASS: Found 0 nonexistent nodes"
    end subroutine test_find_nodes_by_type

    subroutine test_get_ancestor_of_type()
        integer :: ancestor_idx

        print *
        print *, "Testing get_ancestor_of_type..."

        ancestor_idx = get_ancestor_of_type(arena, id2_idx, "program_node")

        if (ancestor_idx /= prog_idx) then
            print *, "  FAIL: Expected program ancestor at", prog_idx, &
                "got", ancestor_idx
            stop 1
        end if

        print *, "  PASS: Found program_node ancestor"

        ancestor_idx = get_ancestor_of_type(arena, id2_idx, "assignment_node")

        if (ancestor_idx /= assign1_idx) then
            print *, "  FAIL: Expected assignment ancestor at", assign1_idx, &
                "got", ancestor_idx
            stop 1
        end if

        print *, "  PASS: Found assignment_node ancestor"

        ancestor_idx = get_ancestor_of_type(arena, prog_idx, "program_node")

        if (ancestor_idx /= 0) then
            print *, "  FAIL: Root node should have no ancestor, got", &
                ancestor_idx
            stop 1
        end if

        print *, "  PASS: Root node has no ancestor"

        ancestor_idx = get_ancestor_of_type(arena, id2_idx, "nonexistent")

        if (ancestor_idx /= 0) then
            print *, "  FAIL: Expected no nonexistent ancestor, got", &
                ancestor_idx
            stop 1
        end if

        print *, "  PASS: No nonexistent ancestor found"
    end subroutine test_get_ancestor_of_type

    subroutine test_has_child_of_type()
        logical :: has_child

        print *
        print *, "Testing has_child_of_type..."

        has_child = has_child_of_type(arena, prog_idx, "identifier_node")

        if (.not. has_child) then
            print *, "  FAIL: Program should have identifier child"
            stop 1
        end if

        print *, "  PASS: Program has identifier child"

        has_child = has_child_of_type(arena, prog_idx, "assignment_node")

        if (.not. has_child) then
            print *, "  FAIL: Program should have assignment child"
            stop 1
        end if

        print *, "  PASS: Program has assignment child"

        has_child = has_child_of_type(arena, assign1_idx, "identifier_node")

        if (.not. has_child) then
            print *, "  FAIL: Assignment should have identifier child"
            stop 1
        end if

        print *, "  PASS: Assignment has identifier child"

        has_child = has_child_of_type(arena, id1_idx, "identifier_node")

        if (has_child) then
            print *, "  FAIL: Leaf identifier should not have children"
            stop 1
        end if

        print *, "  PASS: Leaf identifier has no children"

        has_child = has_child_of_type(arena, prog_idx, "nonexistent")

        if (has_child) then
            print *, "  FAIL: Should not have nonexistent child"
            stop 1
        end if

        print *, "  PASS: No nonexistent child found"
    end subroutine test_has_child_of_type

    subroutine test_get_children()
        integer, allocatable :: children(:)

        print *
        print *, "Testing get_children..."

        children = get_children(arena, prog_idx)

        if (size(children) /= 3) then
            print *, "  FAIL: Expected 3 children of program, found", &
                size(children)
            stop 1
        end if

        print *, "  PASS: Program has", size(children), "children"

        children = get_children(arena, assign1_idx)

        if (size(children) /= 1) then
            print *, "  FAIL: Expected 1 child of assignment, found", &
                size(children)
            stop 1
        end if

        print *, "  PASS: Assignment has", size(children), "child"

        children = get_children(arena, id1_idx)

        if (size(children) /= 0) then
            print *, "  FAIL: Expected 0 children of leaf, found", &
                size(children)
            stop 1
        end if

        print *, "  PASS: Leaf identifier has no children"
    end subroutine test_get_children

    subroutine test_traverse_ast()
        type(traverse_counter_t) :: counter

        print *
        print *, "Testing traverse_ast..."

        counter%count = 0

        call traverse_ast(arena, prog_idx, count_callback, counter)

        if (counter%count /= 5) then
            print *, "  FAIL: Expected 5 nodes traversed, got", counter%count
            stop 1
        end if

        print *, "  PASS: Traversed", counter%count, "nodes"
    end subroutine test_traverse_ast

end program test_ast_traversal_utils
