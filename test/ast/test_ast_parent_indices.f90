program test_ast_parent_indices
    use ast_core
    use parser_control_flow_module
    use parser_state_module
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, &
                          TK_NUMBER, TK_STRING, TK_EOF
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_if_block_parent_indices(test_count, pass_count)
    call test_do_loop_parent_indices(test_count, pass_count)
    call test_nested_blocks_parent_indices(test_count, pass_count)

    print *, "Tests passed: ", pass_count, "/", test_count

    if (pass_count /= test_count) then
        print *, "Some tests failed"
        stop 1
    end if

contains

    subroutine test_if_block_parent_indices(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: if_index, i
        integer, allocatable :: children(:)
        logical :: test_passed

        test_count = test_count + 1
        
        ! Create test tokens for: if (x < 0) then \n valid = .false. \n return \n end if
        allocate(tokens(13))
        tokens(1) = token_t(TK_KEYWORD, "if", 1, 1)
        tokens(2) = token_t(TK_OPERATOR, "(", 1, 4)
        tokens(3) = token_t(TK_IDENTIFIER, "x", 1, 5)
        tokens(4) = token_t(TK_OPERATOR, "<", 1, 7)
        tokens(5) = token_t(TK_NUMBER, "0", 1, 9)
        tokens(6) = token_t(TK_OPERATOR, ")", 1, 10)
        tokens(7) = token_t(TK_KEYWORD, "then", 1, 12)
        tokens(8) = token_t(TK_IDENTIFIER, "valid", 2, 3)
        tokens(9) = token_t(TK_OPERATOR, "=", 2, 9)
        tokens(10) = token_t(TK_STRING, ".false.", 2, 11)
        tokens(11) = token_t(TK_KEYWORD, "return", 3, 3)
        tokens(12) = token_t(TK_KEYWORD, "end if", 4, 1)
        tokens(13) = token_t(TK_EOF, "", 4, 7)

        arena = create_ast_arena()
        parser = create_parser_state(tokens)

        ! Parse the if statement
        if_index = parse_if(parser, arena)
        
        test_passed = .true.
        
        ! Check that the if node exists
        if (if_index <= 0 .or. if_index > arena%size) then
            print *, "ERROR: Invalid if_index returned"
            test_passed = .false.
        else
            ! Get children of the if node
            children = arena%get_children(if_index)
            
            ! Check that children have proper parent indices
            do i = 1, size(children)
                if (arena%entries(children(i))%parent_index /= if_index) then
                    print *, "ERROR: Child ", i, " has parent_index = ", &
                            arena%entries(children(i))%parent_index, &
                            " but should be ", if_index
                    test_passed = .false.
                end if
            end do
            
            ! Check body statements specifically
            if (allocated(arena%entries(if_index)%node)) then
                select type(node => arena%entries(if_index)%node)
                type is (if_node)
                    if (allocated(node%then_body_indices)) then
                        do i = 1, size(node%then_body_indices)
                            if (node%then_body_indices(i) > 0) then
                                if (arena%entries(node%then_body_indices(i))%parent_index == 0) then
                                    print *, "ERROR: Statement in if body has parent_index = 0"
                                    test_passed = .false.
                                end if
                            end if
                        end do
                    end if
                end select
            end if
        end if
        
        if (test_passed) then
            pass_count = pass_count + 1
            print *, "PASS: if block parent indices"
        else
            print *, "FAIL: if block parent indices"
        end if
    end subroutine test_if_block_parent_indices

    subroutine test_do_loop_parent_indices(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: loop_index, i
        integer, allocatable :: children(:)
        logical :: test_passed

        test_count = test_count + 1
        
        ! Create test tokens for: do i = 1, 10 \n x = x + 1 \n end do
        allocate(tokens(14))
        tokens(1) = token_t(TK_KEYWORD, "do", 1, 1)
        tokens(2) = token_t(TK_IDENTIFIER, "i", 1, 4)
        tokens(3) = token_t(TK_OPERATOR, "=", 1, 6)
        tokens(4) = token_t(TK_NUMBER, "1", 1, 8)
        tokens(5) = token_t(TK_OPERATOR, ",", 1, 9)
        tokens(6) = token_t(TK_NUMBER, "10", 1, 11)
        tokens(7) = token_t(TK_IDENTIFIER, "x", 2, 3)
        tokens(8) = token_t(TK_OPERATOR, "=", 2, 5)
        tokens(9) = token_t(TK_IDENTIFIER, "x", 2, 7)
        tokens(10) = token_t(TK_OPERATOR, "+", 2, 9)
        tokens(11) = token_t(TK_NUMBER, "1", 2, 11)
        tokens(12) = token_t(TK_KEYWORD, "end", 3, 1)
        tokens(13) = token_t(TK_KEYWORD, "do", 3, 5)
        tokens(14) = token_t(TK_EOF, "", 3, 7)

        arena = create_ast_arena()
        parser = create_parser_state(tokens)

        ! Parse the do loop
        loop_index = parse_do_loop(parser, arena)
        
        test_passed = .true.
        
        ! Check that the loop node exists
        if (loop_index <= 0 .or. loop_index > arena%size) then
            print *, "ERROR: Invalid loop_index returned"
            test_passed = .false.
        else
            ! Get children of the loop node
            children = arena%get_children(loop_index)
            
            ! Check that children have proper parent indices
            do i = 1, size(children)
                if (arena%entries(children(i))%parent_index /= loop_index) then
                    print *, "ERROR: Child ", i, " has parent_index = ", &
                            arena%entries(children(i))%parent_index, &
                            " but should be ", loop_index
                    test_passed = .false.
                end if
            end do
            
            ! Check body statements specifically
            if (allocated(arena%entries(loop_index)%node)) then
                select type(node => arena%entries(loop_index)%node)
                type is (do_loop_node)
                    if (allocated(node%body_indices)) then
                        do i = 1, size(node%body_indices)
                            if (node%body_indices(i) > 0) then
                                if (arena%entries(node%body_indices(i))%parent_index == 0) then
                                    print *, "ERROR: Statement in do loop body has parent_index = 0"
                                    test_passed = .false.
                                end if
                            end if
                        end do
                    end if
                end select
            end if
        end if
        
        if (test_passed) then
            pass_count = pass_count + 1
            print *, "PASS: do loop parent indices"
        else
            print *, "FAIL: do loop parent indices"
        end if
    end subroutine test_do_loop_parent_indices

    subroutine test_nested_blocks_parent_indices(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: outer_if_index, i, j
        integer, allocatable :: outer_children(:), inner_children(:)
        logical :: test_passed

        test_count = test_count + 1
        
        ! Create test tokens for nested if blocks
        ! if (x > 0) then
        !   if (y > 0) then
        !     z = 1
        !   end if
        ! end if
        allocate(tokens(20))
        tokens(1) = token_t(TK_KEYWORD, "if", 1, 1)
        tokens(2) = token_t(TK_OPERATOR, "(", 1, 4)
        tokens(3) = token_t(TK_IDENTIFIER, "x", 1, 5)
        tokens(4) = token_t(TK_OPERATOR, ">", 1, 7)
        tokens(5) = token_t(TK_NUMBER, "0", 1, 9)
        tokens(6) = token_t(TK_OPERATOR, ")", 1, 10)
        tokens(7) = token_t(TK_KEYWORD, "then", 1, 12)
        tokens(8) = token_t(TK_KEYWORD, "if", 2, 3)
        tokens(9) = token_t(TK_OPERATOR, "(", 2, 6)
        tokens(10) = token_t(TK_IDENTIFIER, "y", 2, 7)
        tokens(11) = token_t(TK_OPERATOR, ">", 2, 9)
        tokens(12) = token_t(TK_NUMBER, "0", 2, 11)
        tokens(13) = token_t(TK_OPERATOR, ")", 2, 12)
        tokens(14) = token_t(TK_KEYWORD, "then", 2, 14)
        tokens(15) = token_t(TK_IDENTIFIER, "z", 3, 5)
        tokens(16) = token_t(TK_OPERATOR, "=", 3, 7)
        tokens(17) = token_t(TK_NUMBER, "1", 3, 9)
        tokens(18) = token_t(TK_KEYWORD, "end if", 4, 3)
        tokens(19) = token_t(TK_KEYWORD, "end if", 5, 1)
        tokens(20) = token_t(TK_EOF, "", 5, 7)

        arena = create_ast_arena()
        parser = create_parser_state(tokens)

        ! Parse the outer if statement
        outer_if_index = parse_if(parser, arena)
        
        test_passed = .true.
        
        ! Check that all nodes have proper parent relationships
        if (outer_if_index <= 0 .or. outer_if_index > arena%size) then
            print *, "ERROR: Invalid outer_if_index returned"
            test_passed = .false.
        else
            ! The outer if should have parent_index = 0 (root)
            if (arena%entries(outer_if_index)%parent_index /= 0) then
                print *, "ERROR: Outer if has parent_index = ", &
                        arena%entries(outer_if_index)%parent_index, &
                        " but should be 0"
                test_passed = .false.
            end if
            
            ! Check the nested structure
            if (allocated(arena%entries(outer_if_index)%node)) then
                select type(node => arena%entries(outer_if_index)%node)
                type is (if_node)
                    if (allocated(node%then_body_indices)) then
                        do i = 1, size(node%then_body_indices)
                            if (node%then_body_indices(i) > 0) then
                                ! Check if this is the inner if
                                if (allocated(arena%entries(node%then_body_indices(i))%node_type)) then
                                    if (arena%entries(node%then_body_indices(i))%node_type == "if_statement") then
                                        ! The inner if should have the outer if as parent
                                        if (arena%entries(node%then_body_indices(i))%parent_index == 0) then
                                            print *, "ERROR: Inner if has parent_index = 0"
                                            test_passed = .false.
                                        end if
                                    end if
                                end if
                            end if
                        end do
                    end if
                end select
            end if
        end if
        
        if (test_passed) then
            pass_count = pass_count + 1
            print *, "PASS: nested blocks parent indices"
        else
            print *, "FAIL: nested blocks parent indices"
        end if
    end subroutine test_nested_blocks_parent_indices

end program test_ast_parent_indices
