program test_associate_node
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_nodes_control, only: associate_node, association_t, create_associate
    use ast_factory, only: push_associate, push_identifier, push_binary_op
    use json_module
    implicit none

    integer :: total_tests, passed_tests
    
    total_tests = 0
    passed_tests = 0
    
    ! Run all tests
    call test_associate_node_creation()
    call test_associate_node_assignment()
    call test_associate_node_json()
    call test_associate_factory()
    call test_associate_arena_push()
    
    ! Report results
    write (*, '(A)') ''
    write (*, '(A,I0,A,I0,A)') 'Passed ', passed_tests, ' out of ', total_tests, ' tests'
    if (passed_tests /= total_tests) then
        stop 1
    end if

contains

    subroutine test_associate_node_creation()
        character(len=*), parameter :: test_name = "test_associate_node_creation"
        type(associate_node) :: node
        type(association_t) :: associations(2)
        integer :: body_indices(3)
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Create associations
        associations(1)%name = "x"
        associations(1)%expr_index = 10
        associations(2)%name = "y"
        associations(2)%expr_index = 20
        
        ! Create body indices
        body_indices = [30, 40, 50]
        
        ! Create node using factory function
        node = create_associate(associations, body_indices, line=5, column=10)
        
        ! Verify node properties
        if (allocated(node%associations) .and. &
            size(node%associations) == 2 .and. &
            node%associations(1)%name == "x" .and. &
            node%associations(1)%expr_index == 10 .and. &
            node%associations(2)%name == "y" .and. &
            node%associations(2)%expr_index == 20 .and. &
            allocated(node%body_indices) .and. &
            size(node%body_indices) == 3 .and. &
            all(node%body_indices == [30, 40, 50]) .and. &
            node%line == 5 .and. &
            node%column == 10) then
            write (*, '(A)') 'PASS'
            passed_tests = passed_tests + 1
        else
            write (*, '(A)') 'FAIL - Node properties incorrect'
        end if
    end subroutine test_associate_node_creation

    subroutine test_associate_node_assignment()
        character(len=*), parameter :: test_name = "test_associate_node_assignment"
        type(associate_node) :: node1, node2
        type(association_t) :: associations(1)
        integer :: body_indices(2)
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Create first node
        associations(1)%name = "z"
        associations(1)%expr_index = 100
        body_indices = [200, 300]
        
        node1 = create_associate(associations, body_indices, line=10, column=20)
        
        ! Test assignment
        node2 = node1
        
        ! Verify deep copy
        if (allocated(node2%associations) .and. &
            size(node2%associations) == 1 .and. &
            node2%associations(1)%name == "z" .and. &
            node2%associations(1)%expr_index == 100 .and. &
            allocated(node2%body_indices) .and. &
            size(node2%body_indices) == 2 .and. &
            all(node2%body_indices == [200, 300]) .and. &
            node2%line == 10 .and. &
            node2%column == 20) then
            write (*, '(A)') 'PASS'
            passed_tests = passed_tests + 1
        else
            write (*, '(A)') 'FAIL - Assignment did not create proper copy'
        end if
    end subroutine test_associate_node_assignment

    subroutine test_associate_node_json()
        character(len=*), parameter :: test_name = "test_associate_node_json"
        type(associate_node) :: node
        type(association_t) :: associations(2)
        integer :: body_indices(1)
        type(json_core) :: json
        type(json_value), pointer :: root, obj
        character(len=:), allocatable :: json_str
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Create node
        associations(1)%name = "a"
        associations(1)%expr_index = 1
        associations(2)%name = "b"
        associations(2)%expr_index = 2
        body_indices = [3]
        
        node = create_associate(associations, body_indices, line=1, column=1)
        
        ! Convert to JSON
        call json%create_object(root, '')
        call node%to_json(json, root)
        call json%print_to_string(root, json_str)
        
        ! Check JSON contains required fields (flexible formatting)
        if (index(json_str, 'associate') > 0 .and. &
            index(json_str, 'associations') > 0 .and. &
            index(json_str, 'a') > 0 .and. &
            index(json_str, 'b') > 0 .and. &
            index(json_str, '1') > 0 .and. &
            index(json_str, '2') > 0 .and. &
            index(json_str, 'body_indices') > 0 .and. &
            index(json_str, 'line') > 0 .and. &
            index(json_str, 'column') > 0) then
            write (*, '(A)') 'PASS'
            passed_tests = passed_tests + 1
        else
            write (*, '(A)') 'FAIL - JSON missing required fields'
            write (error_unit, '(A)') 'JSON output:'
            write (error_unit, '(A)') json_str
        end if
        
        call json%destroy(root)
    end subroutine test_associate_node_json

    subroutine test_associate_factory()
        character(len=*), parameter :: test_name = "test_associate_factory"
        type(association_t) :: empty_assocs(0)
        type(associate_node) :: node
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Test with empty associations (should fail in real usage but factory should handle it)
        node = create_associate(empty_assocs)
        
        ! Since the factory checks size > 0, associations won't be allocated for empty array
        if (.not. allocated(node%associations) .and. &
            .not. allocated(node%body_indices)) then
            write (*, '(A)') 'PASS'
            passed_tests = passed_tests + 1
        else
            write (*, '(A)') 'FAIL - Empty associations should not allocate arrays'
        end if
    end subroutine test_associate_factory

    subroutine test_associate_arena_push()
        character(len=*), parameter :: test_name = "test_associate_arena_push"
        type(ast_arena_t) :: arena
        type(association_t) :: associations(1)
        integer :: body_indices(2)
        integer :: id_index, bin_index, assoc_index
        
        write (*, '(A)', advance='no') test_name // ' ... '
        total_tests = total_tests + 1
        
        ! Create expression nodes in arena
        id_index = push_identifier(arena, "x", 1, 1)
        bin_index = push_binary_op(arena, id_index, id_index, "+", 1, 1)
        
        ! Create association
        associations(1)%name = "sum"
        associations(1)%expr_index = bin_index
        
        ! Create body indices (using the same nodes for simplicity)
        body_indices = [id_index, bin_index]
        
        ! Push associate node to arena
        assoc_index = push_associate(arena, associations, body_indices, 1, 1)
        
        ! Verify node was added
        if (assoc_index > 0 .and. assoc_index <= arena%size) then
            select type (node => arena%entries(assoc_index)%node)
            type is (associate_node)
                if (allocated(node%associations) .and. &
                    size(node%associations) == 1 .and. &
                    node%associations(1)%name == "sum" .and. &
                    node%associations(1)%expr_index == bin_index) then
                    write (*, '(A)') 'PASS'
                    passed_tests = passed_tests + 1
                else
                    write (*, '(A)') 'FAIL - Arena node has incorrect properties'
                end if
            class default
                write (*, '(A)') 'FAIL - Wrong node type in arena'
            end select
        else
            write (*, '(A)') 'FAIL - Invalid index returned'
        end if
    end subroutine test_associate_arena_push

end program test_associate_node