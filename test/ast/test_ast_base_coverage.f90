program test_ast_base_coverage
    use ast_base
    use ast_nodes_core, only: identifier_node, literal_node
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== AST Base Coverage Tests ==="
    print *, ""

    ! Test 1: ast_node_wrapper assignment with allocated nodes
    call test_start("ast_node_wrapper assignment with nodes")
    call test_ast_node_wrapper_assign()

    ! Test 2: ast_node_wrapper assignment with deallocation
    call test_start("ast_node_wrapper assignment with reallocation") 
    call test_ast_node_wrapper_assign_deallocate()

    ! Test 3: ast_node_wrapper assignment with unallocated rhs
    call test_start("ast_node_wrapper assignment with unallocated rhs")
    call test_ast_node_wrapper_assign_unallocated()

    ! Test 4: ast_node_wrapper stack_index handling
    call test_start("ast_node_wrapper stack_index operations")
    call test_ast_node_wrapper_stack_index()

    ! Test 5: Type constants verification
    call test_start("literal type constants")
    call test_literal_constants()

    ! Test 6: Default initialization
    call test_start("default initialization")
    call test_default_initialization()

    call print_results()

contains

    subroutine test_ast_node_wrapper_assign()
        type(ast_node_wrapper) :: lhs, rhs
        type(identifier_node) :: test_node

        ! Create a test node
        test_node%name = "test_var"
        test_node%line = 42
        test_node%column = 10

        ! Allocate and assign to rhs
        allocate(rhs%node, source=test_node)
        rhs%stack_index = 123

        ! Test assignment
        lhs = rhs

        ! Verify assignment worked
        if (.not. allocated(lhs%node)) then
            call test_fail("LHS node should be allocated")
            return
        end if

        if (lhs%stack_index /= 123) then
            call test_fail("Stack index should be copied")
            return
        end if

        ! Verify deep copy
        select type (lhs_node => lhs%node)
        type is (identifier_node)
            if (lhs_node%name /= "test_var") then
                call test_fail("Node name should be copied")
                return
            end if
            if (lhs_node%line /= 42) then
                call test_fail("Line should be copied")
                return
            end if
            if (lhs_node%column /= 10) then
                call test_fail("Column should be copied")
                return
            end if
        class default
            call test_fail("Node should maintain identifier_node type")
            return
        end select

        call test_pass()
    end subroutine test_ast_node_wrapper_assign

    subroutine test_ast_node_wrapper_assign_deallocate()
        type(ast_node_wrapper) :: lhs, rhs
        type(literal_node) :: test_node1, test_node2

        ! Create first node and assign to lhs
        test_node1%value = "old_value"
        test_node1%literal_type = "string"
        test_node1%literal_kind = LITERAL_STRING
        allocate(lhs%node, source=test_node1)
        lhs%stack_index = 100

        ! Create second node and assign to rhs
        test_node2%value = "new_value"
        test_node2%literal_type = "string"
        test_node2%literal_kind = LITERAL_STRING
        allocate(rhs%node, source=test_node2)
        rhs%stack_index = 200

        ! Test reassignment (should deallocate old node)
        lhs = rhs

        ! Verify new assignment
        if (.not. allocated(lhs%node)) then
            call test_fail("LHS node should be allocated after reassignment")
            return
        end if

        if (lhs%stack_index /= 200) then
            call test_fail("Stack index should be updated")
            return
        end if

        select type (lhs_node => lhs%node)
        type is (literal_node)
            if (lhs_node%value /= "new_value") then
                call test_fail("Node should have new value after reassignment")
                return
            end if
        class default
            call test_fail("Node should maintain literal_node type")
            return
        end select

        call test_pass()
    end subroutine test_ast_node_wrapper_assign_deallocate

    subroutine test_ast_node_wrapper_assign_unallocated()
        type(ast_node_wrapper) :: lhs, rhs
        type(identifier_node) :: test_node

        ! Allocate lhs but leave rhs unallocated
        test_node%name = "test"
        allocate(lhs%node, source=test_node)
        lhs%stack_index = 100

        ! rhs is unallocated
        rhs%stack_index = 300

        ! Test assignment with unallocated rhs%node
        lhs = rhs

        ! Verify stack index is copied
        if (lhs%stack_index /= 300) then
            call test_fail("Stack index should be copied even with unallocated node")
            return
        end if

        call test_pass()
    end subroutine test_ast_node_wrapper_assign_unallocated

    subroutine test_ast_node_wrapper_stack_index()
        type(ast_node_wrapper) :: wrapper1, wrapper2

        ! Test default initialization
        if (wrapper1%stack_index /= 0) then
            call test_fail("Default stack index should be 0")
            return
        end if

        ! Test stack index assignment
        wrapper1%stack_index = 42
        wrapper2 = wrapper1

        if (wrapper2%stack_index /= 42) then
            call test_fail("Stack index should be copied during assignment")
            return
        end if

        call test_pass()
    end subroutine test_ast_node_wrapper_stack_index

    subroutine test_literal_constants()
        ! Test that literal type constants are defined correctly
        if (LITERAL_INTEGER /= 1) then
            call test_fail("LITERAL_INTEGER should be 1")
            return
        end if

        if (LITERAL_REAL /= 2) then
            call test_fail("LITERAL_REAL should be 2")
            return
        end if

        if (LITERAL_STRING /= 3) then
            call test_fail("LITERAL_STRING should be 3")
            return
        end if

        if (LITERAL_LOGICAL /= 4) then
            call test_fail("LITERAL_LOGICAL should be 4")
            return
        end if

        if (LITERAL_ARRAY /= 5) then
            call test_fail("LITERAL_ARRAY should be 5")
            return
        end if

        if (LITERAL_COMPLEX /= 6) then
            call test_fail("LITERAL_COMPLEX should be 6")
            return
        end if

        call test_pass()
    end subroutine test_literal_constants

    subroutine test_default_initialization()
        type(ast_node_wrapper) :: wrapper
        type(identifier_node) :: node

        ! Test default values
        if (wrapper%stack_index /= 0) then
            call test_fail("Default stack_index should be 0")
            return
        end if

        if (allocated(wrapper%node)) then
            call test_fail("Default node should not be allocated")
            return
        end if

        if (node%line /= 1) then
            call test_fail("Default line should be 1")
            return
        end if

        if (node%column /= 1) then
            call test_fail("Default column should be 1")
            return
        end if

        call test_pass()
    end subroutine test_default_initialization

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A)', advance='no') "Testing " // test_name // "... "
    end subroutine test_start

    subroutine test_pass()
        passed_tests = passed_tests + 1
        print *, "PASS"
    end subroutine test_pass

    subroutine test_fail(message)
        character(len=*), intent(in) :: message
        print *, "FAIL: " // message
    end subroutine test_fail

    subroutine print_results()
        print *, ""
        print *, "=== Test Results ==="
        write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"
        if (passed_tests == total_tests) then
            print *, "All AST base coverage tests passed!"
        else
            print *, "Some tests failed!"
            stop 1
        end if
    end subroutine print_results

end program test_ast_base_coverage