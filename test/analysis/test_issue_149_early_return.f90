program test_issue_149_early_return
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    call test_single_early_return_pattern()

    if (all_tests_passed) then
        print *, "Issue #149 test PASSED!"
    else
        error stop "Issue #149 test FAILED!"
    end if

contains

    subroutine test_single_early_return_pattern()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_statements_module, only: parse_function_definition
        use cfg_builder_module, only: build_control_flow_graph
        use control_flow_graph_module, only: control_flow_graph_t, find_unreachable_code
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(control_flow_graph_t) :: cfg
        integer :: func_index
        integer, allocatable :: unreachable_nodes(:)
        logical :: final_assignment_reachable
        integer :: i
        
        print *, "Testing issue #149: Early return pattern..."
        
        ! Test case from issue #149
        source = "function validate(x) result(valid)" // new_line('a') // &
                "  integer :: x" // new_line('a') // &
                "  logical :: valid" // new_line('a') // &
                "  if (x < 0) then" // new_line('a') // &
                "    valid = .false." // new_line('a') // &
                "    return" // new_line('a') // &  ! Early return for invalid input
                "  end if" // new_line('a') // &
                "  valid = .true." // new_line('a') // &  ! This should be REACHABLE when x >= 0
                "end function"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        func_index = parse_function_definition(parser, arena)
        
        if (func_index <= 0) then
            print *, "FAILED: Could not parse function"
            all_tests_passed = .false.
            return
        end if
        
        ! Build control flow graph
        cfg = build_control_flow_graph(arena, func_index)
        
        ! Find unreachable code
        unreachable_nodes = find_unreachable_code(cfg)
        
        ! Check if the final assignment (valid = .true.) is marked as unreachable
        final_assignment_reachable = .true.
        if (allocated(unreachable_nodes)) then
            ! The final assignment should NOT be in unreachable nodes
            do i = 1, size(unreachable_nodes)
                ! Check if the unreachable node is the final assignment
                if (arena%entries(unreachable_nodes(i))%node_type == "assignment") then
                    ! This is a simple heuristic - in a real test we'd check the specific assignment
                    ! For now, any assignment marked as unreachable fails this test
                    ! since there's only one assignment after the if block
                    select type (node => arena%entries(unreachable_nodes(i))%node)
                    type is (assignment_node)
                        ! Check if this is the assignment after the if block
                        ! The second assignment should be valid = .true.
                        final_assignment_reachable = .false.
                        print *, "Found unreachable assignment node"
                    class default
                        ! Continue checking
                    end select
                end if
            end do
        end if
        
        if (final_assignment_reachable) then
            print *, "PASSED: Code after conditional block with early return is correctly marked as reachable"
        else
            print *, "FAILED: Code after conditional block with early return incorrectly marked as unreachable"
            all_tests_passed = .false.
        end if
        
    end subroutine test_single_early_return_pattern

end program test_issue_149_early_return