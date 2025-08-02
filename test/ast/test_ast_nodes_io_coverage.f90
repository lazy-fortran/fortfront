program test_ast_nodes_io_coverage
    use ast_nodes_io
    use json_module
    use type_system_hm, only: mono_type_t
    implicit none

    integer :: total_tests, passed_tests

    total_tests = 0
    passed_tests = 0

    print *, "=== AST Nodes I/O Coverage Tests ==="
    print *, ""

    ! Test factory functions
    call test_start("create_print_statement factory")
    call test_create_print_statement()

    ! Test print_statement_node
    call test_start("print_statement_node assignment")
    call test_print_statement_assign()

    call test_start("print_statement_node accept visitor")
    call test_print_statement_accept()

    call test_start("print_statement_node to_json")
    call test_print_statement_to_json()

    ! Test write_statement_node
    call test_start("write_statement_node assignment")
    call test_write_statement_assign()

    call test_start("write_statement_node accept visitor")
    call test_write_statement_accept()

    call test_start("write_statement_node to_json")
    call test_write_statement_to_json()

    ! Test read_statement_node
    call test_start("read_statement_node assignment")
    call test_read_statement_assign()

    call test_start("read_statement_node accept visitor")
    call test_read_statement_accept()

    call test_start("read_statement_node to_json")
    call test_read_statement_to_json()

    ! Test format_descriptor_node
    call test_start("format_descriptor_node assignment")
    call test_format_descriptor_assign()

    call test_start("format_descriptor_node accept visitor")
    call test_format_descriptor_accept()

    call test_start("format_descriptor_node to_json")
    call test_format_descriptor_to_json()

    ! Test edge cases
    call test_start("print_statement with allocated inferred_type")
    call test_print_statement_with_type()

    call test_start("write_statement comprehensive")
    call test_write_statement_comprehensive()

    call test_start("read_statement comprehensive")
    call test_read_statement_comprehensive()

    call test_start("format_descriptor comprehensive")
    call test_format_descriptor_comprehensive()

    call print_results()

contains

    subroutine test_create_print_statement()
        type(print_statement_node) :: node1, node2, node3, node4
        integer, parameter :: test_indices(3) = [1, 2, 3]

        ! Test with all parameters
        node1 = create_print_statement(test_indices, "I0", 10, 20)
        if (.not. allocated(node1%expression_indices)) then
            call test_fail("expression_indices should be allocated")
            return
        end if
        if (size(node1%expression_indices) /= 3) then
            call test_fail("expression_indices should have size 3")
            return
        end if
        if (node1%expression_indices(1) /= 1) then
            call test_fail("expression_indices(1) should be 1")
            return
        end if
        if (node1%format_spec /= "I0") then
            call test_fail("format_spec should be 'I0'")
            return
        end if
        if (node1%line /= 10) then
            call test_fail("line should be 10")
            return
        end if
        if (node1%column /= 20) then
            call test_fail("column should be 20")
            return
        end if

        ! Test with no parameters
        node2 = create_print_statement()
        if (node2%format_spec /= "*") then
            call test_fail("default format_spec should be '*'")
            return
        end if

        ! Test with empty expression_indices
        node3 = create_print_statement(expression_indices=[integer :: ])
        if (allocated(node3%expression_indices)) then
            call test_fail("empty expression_indices should not be allocated")
            return
        end if

        ! Test without format_spec
        node4 = create_print_statement(test_indices)
        if (node4%format_spec /= "*") then
            call test_fail("default format_spec should be '*'")
            return
        end if

        call test_pass()
    end subroutine test_create_print_statement

    subroutine test_print_statement_assign()
        type(print_statement_node) :: lhs, rhs
        integer, parameter :: test_indices(2) = [10, 20]

        ! Set up rhs
        rhs%line = 42
        rhs%column = 15
        rhs%expression_indices = test_indices
        rhs%format_spec = "F8.2"

        ! Test assignment
        lhs = rhs

        ! Verify assignment
        if (lhs%line /= 42) then
            call test_fail("line should be copied")
            return
        end if
        if (lhs%column /= 15) then
            call test_fail("column should be copied")
            return
        end if
        if (.not. allocated(lhs%expression_indices)) then
            call test_fail("expression_indices should be allocated")
            return
        end if
        if (size(lhs%expression_indices) /= 2) then
            call test_fail("expression_indices size should be 2")
            return
        end if
        if (lhs%expression_indices(1) /= 10) then
            call test_fail("expression_indices(1) should be 10")
            return
        end if
        if (lhs%format_spec /= "F8.2") then
            call test_fail("format_spec should be copied")
            return
        end if

        call test_pass()
    end subroutine test_print_statement_assign

    subroutine test_print_statement_accept()
        type(print_statement_node) :: node
        integer :: dummy_visitor

        ! Test accept (stub implementation)
        call node%accept(dummy_visitor)

        call test_pass()
    end subroutine test_print_statement_accept

    subroutine test_print_statement_to_json()
        type(print_statement_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root

        ! Create a simple node
        node%line = 5
        node%column = 10

        ! Test to_json (stub implementation)
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call test_pass()
    end subroutine test_print_statement_to_json

    subroutine test_write_statement_assign()
        type(write_statement_node) :: lhs, rhs
        integer, parameter :: test_indices(3) = [1, 2, 3]

        ! Set up rhs with all fields
        rhs%line = 100
        rhs%column = 200
        rhs%unit_spec = "10"
        rhs%format_spec = "F10.3"
        rhs%arg_indices = test_indices
        rhs%iostat_var_index = 5
        rhs%err_label_index = 999
        rhs%end_label_index = 888
        rhs%format_expr_index = 7
        rhs%is_formatted = .true.

        ! Test assignment
        lhs = rhs

        ! Verify all fields copied
        if (lhs%line /= 100) then
            call test_fail("line should be copied")
            return
        end if
        if (lhs%unit_spec /= "10") then
            call test_fail("unit_spec should be copied")
            return
        end if
        if (lhs%format_spec /= "F10.3") then
            call test_fail("format_spec should be copied")
            return
        end if
        if (size(lhs%arg_indices) /= 3) then
            call test_fail("arg_indices size should be 3")
            return
        end if
        if (lhs%iostat_var_index /= 5) then
            call test_fail("iostat_var_index should be copied")
            return
        end if
        if (lhs%err_label_index /= 999) then
            call test_fail("err_label_index should be copied")
            return
        end if
        if (lhs%end_label_index /= 888) then
            call test_fail("end_label_index should be copied")
            return
        end if
        if (lhs%format_expr_index /= 7) then
            call test_fail("format_expr_index should be copied")
            return
        end if
        if (.not. lhs%is_formatted) then
            call test_fail("is_formatted should be copied")
            return
        end if

        call test_pass()
    end subroutine test_write_statement_assign

    subroutine test_write_statement_accept()
        type(write_statement_node) :: node
        integer :: dummy_visitor

        ! Test accept (stub implementation)
        call node%accept(dummy_visitor)

        call test_pass()
    end subroutine test_write_statement_accept

    subroutine test_write_statement_to_json()
        type(write_statement_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root

        ! Test to_json (stub implementation)
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        call test_pass()
    end subroutine test_write_statement_to_json

    subroutine test_read_statement_assign()
        type(read_statement_node) :: lhs, rhs
        integer, parameter :: test_indices(2) = [10, 20]

        ! Set up rhs
        rhs%line = 50
        rhs%column = 75
        rhs%unit_spec = "*"
        rhs%format_spec = "(A)"
        rhs%var_indices = test_indices
        rhs%iostat_var_index = 3
        rhs%err_label_index = 777
        rhs%end_label_index = 666
        rhs%format_expr_index = 4
        rhs%is_formatted = .true.

        ! Test assignment
        lhs = rhs

        ! Verify fields
        if (lhs%line /= 50) then
            call test_fail("line should be copied")
            return
        end if
        if (lhs%unit_spec /= "*") then
            call test_fail("unit_spec should be copied")
            return
        end if
        if (size(lhs%var_indices) /= 2) then
            call test_fail("var_indices size should be 2")
            return
        end if
        if (lhs%var_indices(2) /= 20) then
            call test_fail("var_indices(2) should be 20")
            return
        end if
        if (lhs%iostat_var_index /= 3) then
            call test_fail("iostat_var_index should be copied")
            return
        end if

        call test_pass()
    end subroutine test_read_statement_assign

    subroutine test_read_statement_accept()
        type(read_statement_node) :: node
        integer :: dummy_visitor

        ! Test accept
        call node%accept(dummy_visitor)

        call test_pass()
    end subroutine test_read_statement_accept

    subroutine test_read_statement_to_json()
        type(read_statement_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node with some data
        node%line = 1
        node%column = 1
        node%unit_spec = "10"
        node%format_spec = "(I0)"
        node%iostat_var_index = 5
        node%err_label_index = 100
        node%end_label_index = 200
        node%format_expr_index = 3
        node%is_formatted = .true.

        ! Test to_json (has actual implementation)
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        ! Verify JSON was created (basic check)
        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_read_statement_to_json

    subroutine test_format_descriptor_assign()
        type(format_descriptor_node) :: lhs, rhs

        ! Set up rhs
        rhs%line = 30
        rhs%column = 40
        rhs%descriptor_type = "F"
        rhs%width = 10
        rhs%decimal_places = 2
        rhs%exponent_width = 3
        rhs%repeat_count = 5
        rhs%is_literal = .true.
        rhs%literal_text = "Hello"

        ! Test assignment
        lhs = rhs

        ! Verify fields
        if (lhs%descriptor_type /= "F") then
            call test_fail("descriptor_type should be copied")
            return
        end if
        if (lhs%width /= 10) then
            call test_fail("width should be copied")
            return
        end if
        if (lhs%decimal_places /= 2) then
            call test_fail("decimal_places should be copied")
            return
        end if
        if (lhs%exponent_width /= 3) then
            call test_fail("exponent_width should be copied")
            return
        end if
        if (lhs%repeat_count /= 5) then
            call test_fail("repeat_count should be copied")
            return
        end if
        if (.not. lhs%is_literal) then
            call test_fail("is_literal should be copied")
            return
        end if
        if (lhs%literal_text /= "Hello") then
            call test_fail("literal_text should be copied")
            return
        end if

        call test_pass()
    end subroutine test_format_descriptor_assign

    subroutine test_format_descriptor_accept()
        type(format_descriptor_node) :: node
        integer :: dummy_visitor

        ! Test accept
        call node%accept(dummy_visitor)

        call test_pass()
    end subroutine test_format_descriptor_accept

    subroutine test_format_descriptor_to_json()
        type(format_descriptor_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_string

        ! Set up node
        node%line = 1
        node%column = 1
        node%descriptor_type = "I"
        node%width = 5
        node%decimal_places = 0
        node%exponent_width = 0
        node%repeat_count = 1
        node%is_literal = .false.

        ! Test to_json (has actual implementation)
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)

        ! Verify JSON was created
        call json%serialize(root, json_string)
        if (.not. allocated(json_string)) then
            call test_fail("JSON string should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_format_descriptor_to_json

    subroutine test_print_statement_with_type()
        type(print_statement_node) :: lhs, rhs
        type(mono_type_t) :: test_type

        ! Set up rhs with inferred_type
        rhs%line = 1
        allocate(rhs%inferred_type)
        rhs%inferred_type = test_type

        ! Test assignment with inferred_type
        lhs = rhs

        if (.not. allocated(lhs%inferred_type)) then
            call test_fail("inferred_type should be allocated")
            return
        end if

        call test_pass()
    end subroutine test_print_statement_with_type

    subroutine test_write_statement_comprehensive()
        type(write_statement_node) :: node1, node2
        type(mono_type_t) :: test_type

        ! Test assignment with inferred_type
        allocate(node1%inferred_type)
        node1%inferred_type = test_type
        node1%unit_spec = "output.txt"

        node2 = node1

        if (.not. allocated(node2%inferred_type)) then
            call test_fail("inferred_type should be copied")
            return
        end if

        ! Test reassignment with deallocation
        deallocate(node1%inferred_type)
        allocate(node1%arg_indices(1))
        node1%arg_indices(1) = 42

        node2 = node1

        if (allocated(node2%inferred_type)) then
            call test_fail("inferred_type should not be allocated after reassignment")
            return
        end if

        if (node2%arg_indices(1) /= 42) then
            call test_fail("arg_indices should be updated")
            return
        end if

        call test_pass()
    end subroutine test_write_statement_comprehensive

    subroutine test_read_statement_comprehensive()
        type(read_statement_node) :: node1, node2
        type(mono_type_t) :: test_type

        ! Test with inferred_type and var_indices
        allocate(node1%inferred_type)
        node1%inferred_type = test_type
        allocate(node1%var_indices(2))
        node1%var_indices = [1, 2]

        node2 = node1

        if (size(node2%var_indices) /= 2) then
            call test_fail("var_indices should be copied")
            return
        end if

        ! Test deallocation during reassignment
        deallocate(node1%var_indices)
        allocate(node1%var_indices(3))
        node1%var_indices = [10, 20, 30]

        node2 = node1

        if (size(node2%var_indices) /= 3) then
            call test_fail("var_indices should be reallocated")
            return
        end if

        if (node2%var_indices(3) /= 30) then
            call test_fail("var_indices(3) should be 30")
            return
        end if

        call test_pass()
    end subroutine test_read_statement_comprehensive

    subroutine test_format_descriptor_comprehensive()
        type(format_descriptor_node) :: node1, node2
        type(mono_type_t) :: test_type

        ! Test all fields including inferred_type
        allocate(node1%inferred_type)
        node1%inferred_type = test_type
        node1%descriptor_type = "E"
        node1%width = 15
        node1%decimal_places = 6
        node1%exponent_width = 2
        node1%repeat_count = 3
        node1%is_literal = .true.
        node1%literal_text = "Format string"

        node2 = node1

        if (node2%descriptor_type /= "E") then
            call test_fail("descriptor_type should be copied")
            return
        end if

        if (node2%literal_text /= "Format string") then
            call test_fail("literal_text should be copied")
            return
        end if

        if (.not. allocated(node2%inferred_type)) then
            call test_fail("inferred_type should be copied")
            return
        end if

        call test_pass()
    end subroutine test_format_descriptor_comprehensive

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
            print *, "All AST nodes I/O coverage tests passed!"
        else
            print *, "Some tests failed!"
            stop 1
        end if
    end subroutine print_results

end program test_ast_nodes_io_coverage