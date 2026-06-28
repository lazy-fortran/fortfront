program test_ast_base_coverage
    use ast_base, only: copy_ast_node_base, LITERAL_INTEGER, LITERAL_REAL, &
        LITERAL_STRING, LITERAL_LOGICAL, LITERAL_ARRAY, &
        LITERAL_COMPLEX, ast_node_wrapper
    use ast_nodes_core, only: identifier_node, literal_node, &
        assignment_node
    use type_system_unified, only: create_mono_type
    use type_constants, only: TINT
    use uid_generator, only: generate_uid
    use, intrinsic :: iso_fortran_env, only: dp => real64
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

    ! Test 7: Copy assignment preserves base fields on identifier_node
    call test_start("copy assignment preserves base fields (identifier_node)")
    call test_copy_assignment_base_fields()

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
        allocate (rhs%node, source=test_node)
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
        allocate (lhs%node, source=test_node1)
        lhs%stack_index = 100

        ! Create second node and assign to rhs
        test_node2%value = "new_value"
        test_node2%literal_type = "string"
        test_node2%literal_kind = LITERAL_STRING
        allocate (rhs%node, source=test_node2)
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
        allocate (lhs%node, source=test_node)
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

        if (allocated(lhs%node)) then
            call test_fail("LHS node should be cleared by unallocated RHS")
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

    subroutine test_copy_assignment_base_fields()
        type(identifier_node) :: src, dst

        ! Set base fields to non-default values
        src%line = 42
        src%column = 17
        src%uid = generate_uid()
        src%inferred_type = create_mono_type(TINT)
        src%is_constant = .true.
        src%constant_logical = .true.
        src%constant_integer = 99
        src%constant_real = 3.14_dp
        src%constant_type = LITERAL_INTEGER
        src%stmt_label = "10"

        ! Set derived field
        src%name = "test_name"

        ! Copy assignment (uses identifier_assign which calls copy_ast_node_base)
        dst = src

        ! Verify base fields survived
        if (dst%line /= 42) then
            call test_fail("line should survive copy assignment")
            return
        end if

        if (dst%column /= 17) then
            call test_fail("column should survive copy assignment")
            return
        end if

        if (dst%uid%value /= src%uid%value) then
            call test_fail("uid should survive copy assignment")
            return
        end if

        if (dst%inferred_type%kind /= TINT) then
            call test_fail("inferred_type kind should survive copy assignment")
            return
        end if

        if (dst%inferred_type%size /= src%inferred_type%size) then
            call test_fail("inferred_type size should survive copy assignment")
            return
        end if

        if (.not. dst%is_constant) then
            call test_fail("is_constant should survive copy assignment")
            return
        end if

        if (.not. dst%constant_logical) then
            call test_fail("constant_logical should survive copy assignment")
            return
        end if

        if (dst%constant_integer /= 99) then
            call test_fail("constant_integer should survive copy assignment")
            return
        end if

        if (dst%constant_real /= 3.14_dp) then
            call test_fail("constant_real should survive copy assignment")
            return
        end if

        if (dst%constant_type /= LITERAL_INTEGER) then
            call test_fail("constant_type should survive copy assignment")
            return
        end if

        if (.not. allocated(dst%stmt_label)) then
            call test_fail("stmt_label should survive copy assignment")
            return
        end if

        if (dst%stmt_label /= "10") then
            call test_fail("stmt_label value should survive copy assignment")
            return
        end if

        ! Verify derived field survived
        if (dst%name /= "test_name") then
            call test_fail("derived field name should survive copy assignment")
            return
        end if

        deallocate (src%stmt_label)
        dst = src
        if (allocated(dst%stmt_label)) then
            call test_fail("stmt_label should be cleared when absent on source")
            return
        end if

        call test_pass()
    end subroutine test_copy_assignment_base_fields

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write (*, '(A)', advance='no') "Testing "//test_name//"... "
    end subroutine test_start

    subroutine test_pass()
        passed_tests = passed_tests + 1
        print *, "PASS"
    end subroutine test_pass

    subroutine test_fail(message)
        character(len=*), intent(in) :: message
        print *, "FAIL: "//message
    end subroutine test_fail

    subroutine print_results()
        print *, ""
        print *, "=== Test Results ==="
        write (*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"
        if (passed_tests == total_tests) then
            print *, "All AST base coverage tests passed!"
        else
            print *, "Some tests failed!"
            stop 1
        end if
    end subroutine print_results

end program test_ast_base_coverage
