program test_issue_1610_recursive_pointers
    use transformation_api, only: transform_lazy_fortran_string

    call test_binary_tree_node()
    call test_linked_list_node()
    call test_recursive_allocatable_array()
    call test_multiple_pointer_children()
    print *, ""
    print *, "All recursive pointer type tests passed"

contains

    subroutine test_binary_tree_node()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg

        input_code = "type :: node" // new_line('A') // &
            "  integer :: value" // new_line('A') // &
            "  type(node), pointer :: left => null()" // new_line('A') // &
            "  type(node), pointer :: right => null()" // new_line('A') // &
            "end type node"

        print *, ""
        print *, "=== Test 1: Binary tree with recursive pointers ==="

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (error_msg /= "") then
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "type(node), pointer :: left => null()") == 0) then
            print *, "FAIL: Left pointer not preserved"
            error stop 1
        end if

        if (index(output_code, "type(node), pointer :: right => null()") == 0) then
            print *, "FAIL: Right pointer not preserved"
            error stop 1
        end if

        print *, "PASS: Binary tree recursive pointers preserved"
    end subroutine test_binary_tree_node

    subroutine test_linked_list_node()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg

        input_code = "type :: list_node" // new_line('A') // &
            "  real :: data" // new_line('A') // &
            "  type(list_node), pointer :: next => " // &
            "null()" // new_line('A') // &
            "end type list_node"

        print *, ""
        print *, "=== Test 2: Linked list with recursive pointer ==="

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (error_msg /= "") then
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "type(list_node), pointer :: next => null()") == 0) then
            print *, "FAIL: Next pointer not preserved"
            error stop 1
        end if

        print *, "PASS: Linked list recursive pointer preserved"
    end subroutine test_linked_list_node

    subroutine test_recursive_allocatable_array()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg

        input_code = "type :: t_node" // new_line('A') // &
            "  integer :: id" // new_line('A') // &
            "  type(t_node), dimension(:), allocatable :: " // &
            "children" // new_line('A') // &
            "end type t_node"

        print *, ""
        print *, "=== Test 3: Tree with recursive allocatable array ==="

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (error_msg /= "") then
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "children") == 0) then
            print *, "FAIL: Children array not preserved"
            error stop 1
        end if

        if (index(output_code, "allocatable") == 0) then
            print *, "FAIL: Allocatable attribute not preserved"
            error stop 1
        end if

        print *, "PASS: Recursive allocatable array preserved"
    end subroutine test_recursive_allocatable_array

    subroutine test_multiple_pointer_children()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg

        input_code = "type :: graph_node" // new_line('A') // &
            "  character(len=32) :: label" // new_line('A') // &
            "  type(graph_node), pointer :: parent => " // &
            "null()" // new_line('A') // &
            "  type(graph_node), pointer :: first_child => " // &
            "null()" // new_line('A') // &
            "  type(graph_node), pointer :: next_sibling => " // &
            "null()" // new_line('A') // &
            "end type graph_node"

        print *, ""
        print *, "=== Test 4: Graph node with multiple recursive pointers ==="

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (error_msg /= "") then
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "parent => null()") == 0) then
            print *, "FAIL: Parent pointer not preserved"
            error stop 1
        end if

        if (index(output_code, "first_child => null()") == 0) then
            print *, "FAIL: First child pointer not preserved"
            error stop 1
        end if

        if (index(output_code, "next_sibling => null()") == 0) then
            print *, "FAIL: Next sibling pointer not preserved"
            error stop 1
        end if

        print *, "PASS: Multiple recursive pointers preserved"
    end subroutine test_multiple_pointer_children

end program test_issue_1610_recursive_pointers
