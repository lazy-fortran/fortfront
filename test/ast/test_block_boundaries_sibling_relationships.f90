program test_block_boundaries_sibling_relationships
    use fortfront, only: ast_arena_t, create_ast_arena, &
                         get_next_sibling, get_previous_sibling, &
                         get_block_statements, is_last_in_block, is_block_node
    use ast_factory, only: push_program, push_assignment, push_if, &
                           push_identifier, push_literal
    use ast_types, only: LITERAL_INTEGER
    implicit none

    logical :: all_tests_passed = .true.

    print *, "=== Block Boundaries and Sibling Relationships Tests ==="
    print *, ""

    call test_sibling_navigation()
    call test_block_detection()
    call test_last_in_block_detection()
    call test_block_statements()

    if (all_tests_passed) then
        print *, ""
        print *, "All block boundaries and sibling relationship tests passed!"
    else
        print *, ""
        print *, "Some tests failed!"
        stop 1
    end if

contains

    subroutine test_sibling_navigation()
        type(ast_arena_t) :: arena
        integer :: prog_idx, stmt1_idx, stmt2_idx, stmt3_idx
        integer :: var_idx, val_idx, next_idx, prev_idx
        integer, allocatable :: empty_body(:)

        print *, "Testing sibling navigation..."

        ! Create a simple program with three statements
        arena = create_ast_arena()
        
        ! Create program node with empty body initially
        allocate(empty_body(0))
        prog_idx = push_program(arena, "test_prog", empty_body, 1, 1)
        
        ! Create three assignment statements as children of the program
        var_idx = push_identifier(arena, "a", 1, 1)
        val_idx = push_literal(arena, "1", LITERAL_INTEGER, 1, 1)
        stmt1_idx = push_assignment(arena, var_idx, val_idx, 1, 1, prog_idx)
        
        var_idx = push_identifier(arena, "b", 2, 1)
        val_idx = push_literal(arena, "2", LITERAL_INTEGER, 2, 1)
        stmt2_idx = push_assignment(arena, var_idx, val_idx, 2, 1, prog_idx)
        
        var_idx = push_identifier(arena, "c", 3, 1)
        val_idx = push_literal(arena, "3", LITERAL_INTEGER, 3, 1)
        stmt3_idx = push_assignment(arena, var_idx, val_idx, 3, 1, prog_idx)

        ! Test get_next_sibling
        next_idx = get_next_sibling(arena, stmt1_idx)
        if (next_idx == stmt2_idx) then
            print *, "  ✓ get_next_sibling: stmt1 -> stmt2"
        else
            print *, "  ✗ get_next_sibling: Expected", stmt2_idx, "got", next_idx
            all_tests_passed = .false.
        end if

        next_idx = get_next_sibling(arena, stmt2_idx)
        if (next_idx == stmt3_idx) then
            print *, "  ✓ get_next_sibling: stmt2 -> stmt3"
        else
            print *, "  ✗ get_next_sibling: Expected", stmt3_idx, "got", next_idx
            all_tests_passed = .false.
        end if

        ! Test get_previous_sibling
        prev_idx = get_previous_sibling(arena, stmt2_idx)
        if (prev_idx == stmt1_idx) then
            print *, "  ✓ get_previous_sibling: stmt2 -> stmt1"
        else
            print *, "  ✗ get_previous_sibling: Expected", stmt1_idx, "got", prev_idx
            all_tests_passed = .false.
        end if

        prev_idx = get_previous_sibling(arena, stmt3_idx)
        if (prev_idx == stmt2_idx) then
            print *, "  ✓ get_previous_sibling: stmt3 -> stmt2"
        else
            print *, "  ✗ get_previous_sibling: Expected", stmt2_idx, "got", prev_idx
            all_tests_passed = .false.
        end if

        ! Test boundary conditions
        next_idx = get_next_sibling(arena, stmt3_idx)  ! Last sibling
        if (next_idx == 0) then
            print *, "  ✓ get_next_sibling: Last sibling returns 0"
        else
            print *, "  ✗ get_next_sibling: Expected 0 for last sibling, got", next_idx
            all_tests_passed = .false.
        end if

        prev_idx = get_previous_sibling(arena, stmt1_idx)  ! First sibling
        if (prev_idx == 0) then
            print *, "  ✓ get_previous_sibling: First sibling returns 0"
        else
            print *, "  ✗ get_previous_sibling: Expected 0 for first sibling, got", prev_idx
            all_tests_passed = .false.
        end if
    end subroutine test_sibling_navigation

    subroutine test_block_detection()
        type(ast_arena_t) :: arena
        integer :: prog_idx, if_idx, stmt_idx
        integer :: cond_idx, var_idx, val_idx
        integer, allocatable :: empty_body(:)

        print *, "Testing block detection..."

        arena = create_ast_arena()
        
        ! Create program and if nodes for testing
        allocate(empty_body(0))
        prog_idx = push_program(arena, "test_prog", empty_body, 1, 1)
        
        ! Create an if statement with a condition and body
        cond_idx = push_literal(arena, ".true.", LITERAL_INTEGER, 1, 1)  ! Simple condition
        if_idx = push_if(arena, cond_idx, empty_body, empty_body, empty_body, 1, 1, prog_idx)
        
        ! Create assignment in if body
        var_idx = push_identifier(arena, "x", 2, 1)
        val_idx = push_literal(arena, "42", LITERAL_INTEGER, 2, 1)
        stmt_idx = push_assignment(arena, var_idx, val_idx, 2, 1, if_idx)

        ! Test is_block_node
        if (is_block_node(arena, prog_idx)) then
            print *, "  ✓ is_block_node: program_node recognized as block"
        else
            print *, "  ✗ is_block_node: program_node should be a block"
            all_tests_passed = .false.
        end if

        if (is_block_node(arena, if_idx)) then
            print *, "  ✓ is_block_node: if_node recognized as block"
        else
            print *, "  ✗ is_block_node: if_node should be a block"
            all_tests_passed = .false.
        end if

        if (.not. is_block_node(arena, stmt_idx)) then
            print *, "  ✓ is_block_node: assignment_node not a block"
        else
            print *, "  ✗ is_block_node: assignment_node should not be a block"
            all_tests_passed = .false.
        end if
    end subroutine test_block_detection

    subroutine test_last_in_block_detection()
        type(ast_arena_t) :: arena
        integer :: prog_idx, stmt1_idx, stmt2_idx, stmt3_idx
        integer :: var_idx, val_idx
        integer, allocatable :: empty_body(:)

        print *, "Testing last-in-block detection..."

        arena = create_ast_arena()
        
        ! Create program with statements
        allocate(empty_body(0))
        prog_idx = push_program(arena, "test_prog", empty_body, 1, 1)
        
        ! Create three assignment statements
        var_idx = push_identifier(arena, "a", 1, 1)
        val_idx = push_literal(arena, "1", LITERAL_INTEGER, 1, 1)
        stmt1_idx = push_assignment(arena, var_idx, val_idx, 1, 1, prog_idx)
        
        var_idx = push_identifier(arena, "b", 2, 1)
        val_idx = push_literal(arena, "2", LITERAL_INTEGER, 2, 1)
        stmt2_idx = push_assignment(arena, var_idx, val_idx, 2, 1, prog_idx)
        
        var_idx = push_identifier(arena, "c", 3, 1)
        val_idx = push_literal(arena, "3", LITERAL_INTEGER, 3, 1)
        stmt3_idx = push_assignment(arena, var_idx, val_idx, 3, 1, prog_idx)

        ! Test is_last_in_block
        if (.not. is_last_in_block(arena, stmt1_idx)) then
            print *, "  ✓ is_last_in_block: First statement not last"
        else
            print *, "  ✗ is_last_in_block: First statement should not be last"
            all_tests_passed = .false.
        end if

        if (.not. is_last_in_block(arena, stmt2_idx)) then
            print *, "  ✓ is_last_in_block: Middle statement not last"
        else
            print *, "  ✗ is_last_in_block: Middle statement should not be last"
            all_tests_passed = .false.
        end if

        if (is_last_in_block(arena, stmt3_idx)) then
            print *, "  ✓ is_last_in_block: Last statement is last"
        else
            print *, "  ✗ is_last_in_block: Last statement should be last"
            all_tests_passed = .false.
        end if
    end subroutine test_last_in_block_detection

    subroutine test_block_statements()
        type(ast_arena_t) :: arena
        integer :: prog_idx, stmt1_idx, stmt2_idx, stmt3_idx
        integer :: var_idx, val_idx
        integer, allocatable :: statements(:), empty_body(:)

        print *, "Testing block statements retrieval..."

        arena = create_ast_arena()
        
        ! Create program with statements
        allocate(empty_body(0))
        prog_idx = push_program(arena, "test_prog", empty_body, 1, 1)
        
        ! Create three assignment statements
        var_idx = push_identifier(arena, "a", 1, 1)
        val_idx = push_literal(arena, "1", LITERAL_INTEGER, 1, 1)
        stmt1_idx = push_assignment(arena, var_idx, val_idx, 1, 1, prog_idx)
        
        var_idx = push_identifier(arena, "b", 2, 1)
        val_idx = push_literal(arena, "2", LITERAL_INTEGER, 2, 1)
        stmt2_idx = push_assignment(arena, var_idx, val_idx, 2, 1, prog_idx)
        
        var_idx = push_identifier(arena, "c", 3, 1)
        val_idx = push_literal(arena, "3", LITERAL_INTEGER, 3, 1)
        stmt3_idx = push_assignment(arena, var_idx, val_idx, 3, 1, prog_idx)

        ! Test get_block_statements
        statements = get_block_statements(arena, prog_idx)
        
        if (size(statements) == 3) then
            print *, "  ✓ get_block_statements: Correct number of statements"
        else
            print *, "  ✗ get_block_statements: Expected 3 statements, got", size(statements)
            all_tests_passed = .false.
        end if

        if (size(statements) >= 3) then
            if (statements(1) == stmt1_idx .and. &
                statements(2) == stmt2_idx .and. &
                statements(3) == stmt3_idx) then
                print *, "  ✓ get_block_statements: Statements in correct order"
            else
                print *, "  ✗ get_block_statements: Statements not in expected order"
                all_tests_passed = .false.
            end if
        end if
    end subroutine test_block_statements

end program test_block_boundaries_sibling_relationships