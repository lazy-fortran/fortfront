program test_node_type_identification
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    call test_node_type_constants()
    call test_get_node_type_function()
    call test_get_node_type_id_function()
    call test_comment_node_support()

    if (all_tests_passed) then
        print *, "All node type identification tests PASSED!"
    else
        error stop "Some node type identification tests FAILED!"
    end if

contains

    subroutine test_node_type_constants()
        print *, "Testing node type constants..."
        
        ! Test that all required constants are defined
        if (NODE_PROGRAM /= 1) then
            print *, "FAILED: NODE_PROGRAM constant incorrect"
            all_tests_passed = .false.
            return
        end if
        
        if (NODE_ASSIGNMENT /= 3) then
            print *, "FAILED: NODE_ASSIGNMENT constant incorrect"
            all_tests_passed = .false.
            return
        end if
        
        if (NODE_COMMENT /= 40) then
            print *, "FAILED: NODE_COMMENT constant missing or incorrect"
            all_tests_passed = .false.
            return
        end if
        
        if (NODE_UNKNOWN /= 99) then
            print *, "FAILED: NODE_UNKNOWN constant incorrect"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Node type constants test"
    end subroutine test_node_type_constants

    subroutine test_get_node_type_function()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, i, node_type
        logical :: found_program, found_assignment
        
        print *, "Testing get_node_type function..."
        
        ! Create test source with known node types
        source = "program test" // new_line('a') // &
                "    integer :: x = 42" // new_line('a') // &
                "    x = x + 1" // new_line('a') // &
                "end program test"
        
        ! Parse source
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Test node type identification
        found_program = .false.
        found_assignment = .false.
        
        do i = 1, arena%size
            node_type = get_node_type(arena, i)
            
            if (node_type == NODE_PROGRAM) then
                found_program = .true.
            else if (node_type == NODE_ASSIGNMENT) then
                found_assignment = .true.
            end if
        end do
        
        if (.not. found_program) then
            print *, "FAILED: Did not find expected program node"
            all_tests_passed = .false.
            return
        end if
        
        if (.not. found_assignment) then
            print *, "FAILED: Did not find expected assignment node"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: get_node_type function test"
    end subroutine test_get_node_type_function

    subroutine test_get_node_type_id_function()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, i, type_id
        logical :: found_program
        
        print *, "Testing get_node_type_id function..."
        
        ! Create simple test source
        source = "program simple" // new_line('a') // &
                "end program simple"
        
        ! Parse source
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Test direct node type identification
        found_program = .false.
        
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                type_id = get_node_type_id(arena%entries(i)%node)
                
                if (type_id == NODE_PROGRAM) then
                    found_program = .true.
                end if
            end if
        end do
        
        if (.not. found_program) then
            print *, "FAILED: get_node_type_id did not find program node"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: get_node_type_id function test"
    end subroutine test_get_node_type_id_function

    subroutine test_comment_node_support()
        print *, "Testing comment node support..."
        
        ! Test that NODE_COMMENT constant is defined
        if (NODE_COMMENT == NODE_UNKNOWN) then
            print *, "FAILED: NODE_COMMENT not properly defined"
            all_tests_passed = .false.
            return
        end if
        
        ! Note: We would need to create a comment node to fully test this,
        ! but for now we just verify the constant exists
        print *, "PASSED: Comment node support test"
    end subroutine test_comment_node_support

end program test_node_type_identification