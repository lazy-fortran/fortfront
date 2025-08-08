program test_issue_161_early_return
    use fortfront
    use cfg_builder_module, only: build_control_flow_graph
    use control_flow_graph_module, only: control_flow_graph_t, find_unreachable_code
    use lexer_core, only: tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_statements_module, only: parse_function_definition
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    ! Test the exact case mentioned in the issue
    call test_early_return_from_issue(test_count, pass_count)

    print *, "Tests passed: ", pass_count, "/", test_count

    if (pass_count /= test_count) then
        error stop "Some tests failed"
    end if

contains

    subroutine test_early_return_from_issue(test_count, pass_count)
        integer, intent(inout) :: test_count, pass_count
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(control_flow_graph_t) :: cfg
        integer :: func_index
        integer, allocatable :: unreachable_nodes(:)
        logical :: test_passed
        integer :: i
        logical :: found_unreachable_assignment

        test_count = test_count + 1
        test_passed = .true.
        
        ! This is the exact test case from issue #161
        source = "function validate(x) result(valid)" // new_line('a') // &
                "  integer :: x" // new_line('a') // &
                "  logical :: valid" // new_line('a') // &
                "  if (x < 0) then" // new_line('a') // &
                "    valid = .false." // new_line('a') // &
                "    return" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  valid = .true." // new_line('a') // &  ! This should be detected as conditionally unreachable
                "end function"

        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)

        func_index = parse_function_definition(parser, arena)
        
        if (func_index <= 0) then
            print *, "FAIL: Could not parse function"
            return
        end if

        ! Build control flow graph
        cfg = build_control_flow_graph(arena, func_index)

        ! Find unreachable code
        unreachable_nodes = find_unreachable_code(cfg)

        ! After the enhancement, find_unreachable_code should detect 
        ! conditionally unreachable code in early return patterns
        found_unreachable_assignment = .false.
        if (allocated(unreachable_nodes)) then
            do i = 1, size(unreachable_nodes)
                if (allocated(arena%entries(unreachable_nodes(i))%node_type)) then
                    if (arena%entries(unreachable_nodes(i))%node_type == "assignment" .or. &
                        arena%entries(unreachable_nodes(i))%node_type == "multi_declaration") then
                        found_unreachable_assignment = .true.
                    end if
                end if
            end do
        end if

        if (found_unreachable_assignment) then
            print *, "PASS: find_unreachable_code detected conditionally unreachable code"
            pass_count = pass_count + 1
        else
            print *, "FAIL: find_unreachable_code should now detect early return patterns"
            print *, "  After enhancement, it should combine absolutely and conditionally unreachable code"
        end if
        
    end subroutine test_early_return_from_issue

end program test_issue_161_early_return