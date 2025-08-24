program test_cst_enhanced_features
    ! Test enhanced CST features: children management, text content, trivia handling
    use cst_nodes
    use cst_arena
    use cst_core
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== CST Enhanced Features Tests ==="
    print *, ""

    ! Test 1: Create CST node with text content
    call test_start("Create CST node with text content")
    block
        type(cst_node_t) :: node
        
        node = create_cst_node(CST_IDENTIFIER, 5, 15, "variable_name")
        
        if (node%kind == CST_IDENTIFIER .and. &
            node%start_pos == 5 .and. &
            node%end_pos == 15 .and. &
            allocated(node%text) .and. &
            node%text == "variable_name" .and. &
            node%ast_link == 0) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: CST_IDENTIFIER with text 'variable_name', ast_link=0"
            if (allocated(node%text)) then
                print *, "  Got: kind=", node%kind, ", text='", node%text, "', ast_link=", node%ast_link
            else
                print *, "  Got: kind=", node%kind, ", text=<unallocated>, ast_link=", node%ast_link
            end if
        end if
    end block

    ! Test 2: Add children to CST node
    call test_start("Add children to CST node")
    block
        type(cst_node_t) :: parent_node, child1, child2
        type(cst_arena_t) :: arena
        type(cst_handle_t) :: parent_handle, child1_handle, child2_handle
        
        arena = create_cst_arena(10)
        
        ! Create parent and children
        parent_node = create_cst_node(CST_ASSIGNMENT, 1, 20)
        child1 = create_cst_node(CST_IDENTIFIER, 1, 10, "x")
        child2 = create_cst_node(CST_LITERAL, 15, 20, "42")
        
        ! Add to arena
        parent_handle = arena%push(parent_node)
        child1_handle = arena%push(child1)
        child2_handle = arena%push(child2)
        
        ! Add children to parent (modify the node in arena)
        parent_node = arena%get(parent_handle)
        call add_child_to_cst_node(parent_node, child1_handle%index)
        call add_child_to_cst_node(parent_node, child2_handle%index)
        
        if (allocated(parent_node%children) .and. &
            size(parent_node%children) == 2 .and. &
            parent_node%children(1) == child1_handle%index .and. &
            parent_node%children(2) == child2_handle%index) then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: 2 children with correct indices"
            if (allocated(parent_node%children)) then
                print *, "  Got:", size(parent_node%children), "children"
            else
                print *, "  Got: no children allocated"
            end if
        end if
    end block

    ! Test 3: Add trivia to CST nodes
    call test_start("Add trivia to CST nodes")
    block
        type(cst_node_t) :: node
        type(trivia_t) :: leading_comment, trailing_comment, whitespace
        
        node = create_cst_node(CST_DECLARATION, 10, 30)
        leading_comment = create_trivia(CST_COMMENT, "! Variable declaration", 1, 21)
        whitespace = create_trivia(CST_WHITESPACE, "    ", 22, 25)
        trailing_comment = create_trivia(CST_COMMENT, "! End of line", 31, 43)
        
        call add_leading_trivia(node, leading_comment)
        call add_leading_trivia(node, whitespace)
        call add_trailing_trivia(node, trailing_comment)
        
        if (allocated(node%leading_trivia) .and. &
            allocated(node%trailing_trivia) .and. &
            size(node%leading_trivia) == 2 .and. &
            size(node%trailing_trivia) == 1 .and. &
            node%leading_trivia(1)%text == "! Variable declaration" .and. &
            node%leading_trivia(2)%text == "    " .and. &
            node%trailing_trivia(1)%text == "! End of line") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: 2 leading trivia, 1 trailing trivia with correct content"
            if (allocated(node%leading_trivia)) then
                print *, "  Got:", size(node%leading_trivia), "leading trivia"
            end if
            if (allocated(node%trailing_trivia)) then
                print *, "  Got:", size(node%trailing_trivia), "trailing trivia"
            end if
        end if
    end block

    ! Test 4: Set text content for existing node
    call test_start("Set text content for existing node")
    block
        type(cst_node_t) :: node
        
        node = create_cst_node(CST_LITERAL, 5, 10)
        call set_cst_node_text(node, "hello")
        
        if (allocated(node%text) .and. node%text == "hello") then
            call test_pass()
        else
            call test_fail()
            print *, "  Expected: text = 'hello'"
            if (allocated(node%text)) then
                print *, "  Got: text = '", node%text, "'"
            else
                print *, "  Got: text = <unallocated>"
            end if
        end if
    end block

    ! Test 5: Assignment operator with enhanced fields
    call test_start("Assignment operator with enhanced fields")
    block
        type(cst_node_t) :: original, copy
        type(trivia_t) :: comment
        
        ! Create original node with all features
        original = create_cst_node(CST_FUNCTION, 1, 50, "my_function")
        original%ast_link = 42
        
        call add_child_to_cst_node(original, 10)
        call add_child_to_cst_node(original, 20)
        
        comment = create_trivia(CST_COMMENT, "! Function comment", 1, 18)
        call add_leading_trivia(original, comment)
        
        ! Test assignment
        copy = original
        
        if (copy%kind == CST_FUNCTION .and. &
            copy%ast_link == 42 .and. &
            allocated(copy%text) .and. copy%text == "my_function" .and. &
            allocated(copy%children) .and. size(copy%children) == 2 .and. &
            copy%children(1) == 10 .and. copy%children(2) == 20 .and. &
            allocated(copy%leading_trivia) .and. size(copy%leading_trivia) == 1 .and. &
            copy%leading_trivia(1)%text == "! Function comment") then
            call test_pass()
        else
            call test_fail()
            print *, "  Assignment did not properly copy all fields"
        end if
    end block

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All CST enhanced features tests passed!"
        stop 0
    else
        print *, "Some CST enhanced features tests failed!"
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

end program test_cst_enhanced_features