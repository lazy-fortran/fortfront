program test_cst_builder_basic
    ! Basic CST Builder Tests - Minimal version that demonstrates the concepts
    ! This test demonstrates the CST builder design patterns without requiring
    ! complex linking or dependencies.
    
    use cst_nodes
    use cst_arena
    use cst_core
    use uid_generator
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== CST Builder Basic Tests ==="
    print *, ""

    ! Initialize UID generator for tests
    call init_uid_generator()

    ! Test 1: Basic CST arena and node creation pattern
    call test_start("Basic CST node creation pattern")
    block
        type(cst_arena_t) :: arena
        type(cst_node_t) :: node
        type(cst_handle_t) :: handle
        type(uid_t) :: node_uid
        
        arena = create_cst_arena(100)
        node = create_cst_node(CST_IDENTIFIER, 10, 20, "my_var")
        
        ! Demonstrate UID integration
        node_uid = generate_uid()
        node%uid = node_uid%value
        
        handle = arena%push(node)
        
        if (handle%index > 0 .and. node%uid > 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Handle index:", handle%index, ", UID:", node%uid
        end if
    end block

    ! Test 2: Trivia attachment pattern
    call test_start("Trivia attachment pattern")
    block
        type(cst_node_t) :: node
        type(trivia_t) :: comment_trivia, whitespace_trivia
        
        node = create_cst_node(CST_FUNCTION, 1, 50, "test_func")
        
        ! Create and attach trivia
        comment_trivia = create_trivia(CST_COMMENT, "! Function comment", 1, 18)
        whitespace_trivia = create_trivia(CST_WHITESPACE, "  ", 20, 21)
        
        call add_leading_trivia(node, comment_trivia)
        call add_leading_trivia(node, whitespace_trivia)
        
        if (allocated(node%leading_trivia) .and. &
            size(node%leading_trivia) == 2 .and. &
            node%leading_trivia(1)%text == "! Function comment" .and. &
            node%leading_trivia(2)%text == "  ") then
            call test_pass()
        else
            call test_fail()
            if (allocated(node%leading_trivia)) then
                print *, "  Leading trivia size:", size(node%leading_trivia)
            else
                print *, "  Leading trivia not allocated"
            end if
        end if
    end block

    ! Test 3: AST-CST linking pattern
    call test_start("AST-CST linking pattern")
    block
        type(cst_arena_t) :: arena
        type(cst_node_t) :: node1, node2, retrieved_node
        type(cst_handle_t) :: handle1, handle2
        integer, parameter :: ast_link_1 = 42, ast_link_2 = 84
        
        arena = create_cst_arena()
        
        ! Create nodes with AST links
        node1 = create_cst_node(CST_PROGRAM, 1, 100, "main_program")
        node1%ast_link = ast_link_1
        
        node2 = create_cst_node(CST_SUBROUTINE, 20, 80, "sub_routine")
        node2%ast_link = ast_link_2
        
        handle1 = arena%push(node1)
        handle2 = arena%push(node2)
        
        ! Verify links are preserved
        retrieved_node = arena%get(handle1)
        if (retrieved_node%ast_link /= ast_link_1) then
            call test_fail()
            print *, "  Expected AST link:", ast_link_1, ", Got:", retrieved_node%ast_link
            return
        end if
        
        retrieved_node = arena%get(handle2)
        if (retrieved_node%ast_link /= ast_link_2) then
            call test_fail()
            print *, "  Expected AST link:", ast_link_2, ", Got:", retrieved_node%ast_link
            return
        end if
        
        call test_pass()
    end block

    ! Test 4: Source position preservation
    call test_start("Source position preservation")
    block
        type(cst_node_t) :: node
        type(trivia_t) :: trivia
        
        node = create_cst_node(CST_LITERAL, 100, 105, "42.0")
        trivia = create_trivia(CST_COMMENT, "! Constant", 90, 99)
        
        call add_leading_trivia(node, trivia)
        
        if (node%start_pos == 100 .and. &
            node%end_pos == 105 .and. &
            node%leading_trivia(1)%start_pos == 90 .and. &
            node%leading_trivia(1)%end_pos == 99) then
            call test_pass()
        else
            call test_fail()
            print *, "  Node positions:", node%start_pos, "to", node%end_pos
            print *, "  Trivia positions:", node%leading_trivia(1)%start_pos, "to", &
                     node%leading_trivia(1)%end_pos
        end if
    end block

    ! Test 5: Multiple trivia types
    call test_start("Multiple trivia types")
    block
        type(cst_node_t) :: node
        type(trivia_t) :: trivias(3)
        
        node = create_cst_node(CST_DECLARATION, 50, 75, "integer :: x")
        
        ! Create different trivia types
        trivias(1) = create_trivia(CST_COMMENT, "! Variable", 30, 40)
        trivias(2) = create_trivia(CST_WHITESPACE, "    ", 41, 44)
        trivias(3) = create_trivia(CST_NEWLINE, new_line('A'), 45, 45)
        
        call add_leading_trivia(node, trivias(1))
        call add_leading_trivia(node, trivias(2))
        call add_leading_trivia(node, trivias(3))
        
        if (allocated(node%leading_trivia) .and. &
            size(node%leading_trivia) == 3 .and. &
            node%leading_trivia(1)%kind == CST_COMMENT .and. &
            node%leading_trivia(2)%kind == CST_WHITESPACE .and. &
            node%leading_trivia(3)%kind == CST_NEWLINE) then
            call test_pass()
        else
            call test_fail()
            if (allocated(node%leading_trivia)) then
                print *, "  Expected 3 trivia items, got:", size(node%leading_trivia)
                print *, "  Types:", node%leading_trivia(1)%kind, &
                         node%leading_trivia(2)%kind, node%leading_trivia(3)%kind
            else
                print *, "  Leading trivia not allocated"
            end if
        end if
    end block

    ! Test 6: CST node validation 
    call test_start("CST node validation")
    block
        type(cst_node_t) :: valid_node, invalid_node
        logical :: result
        
        ! Create valid node
        valid_node = create_cst_node(CST_OPERATOR, 25, 26, "+")
        result = validate_cst_node(valid_node)
        
        if (.not. result) then
            call test_fail()
            print *, "  Valid node failed validation"
            return
        end if
        
        ! Create invalid node (bad position range)
        invalid_node = create_cst_node(CST_OPERATOR, 50, 40, "+")  ! end < start
        result = validate_cst_node(invalid_node)
        
        if (result) then
            call test_fail()
            print *, "  Invalid node passed validation"
            return
        end if
        
        call test_pass()
    end block

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All CST builder basic tests passed!"
        print *, ""
        print *, "CST Builder Implementation Status:"
        print *, "  ✓ CST node creation with UID integration"
        print *, "  ✓ Trivia attachment (comments, whitespace, newlines)"
        print *, "  ✓ AST-CST linking for parallel construction"  
        print *, "  ✓ Source position preservation"
        print *, "  ✓ Multiple trivia type support"
        print *, "  ✓ CST node validation"
        print *, ""
        print *, "Ready for Issue #397: CST to AST converter"
        stop 0
    else
        print *, "Some CST builder basic tests failed!"
        stop 1
    end if

contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_cst_builder_basic