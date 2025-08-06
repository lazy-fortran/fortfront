program test_issue_149_comprehensive
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    ! Test all cases from issue #149
    call test_early_return_in_single_if()
    call test_early_return_with_else_branch()
    call test_early_return_nested_if()
    call test_early_return_after_if_else()

    if (all_tests_passed) then
        print *, "All issue #149 tests PASSED!"
    else
        error stop "Some issue #149 tests FAILED!"
    end if

contains

    subroutine test_early_return_in_single_if()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_statements_module, only: parse_function_definition
        use cfg_builder_module, only: build_control_flow_graph
        use control_flow_graph_module, only: control_flow_graph_t, get_unreachable_statements
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(control_flow_graph_t) :: cfg
        integer :: func_index
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Testing early return in single if (Issue #149 original case)..."
        
        source = "function validate(x) result(valid)" // new_line('a') // &
                "  integer :: x" // new_line('a') // &
                "  logical :: valid" // new_line('a') // &
                "  if (x < 0) then" // new_line('a') // &
                "    valid = .false." // new_line('a') // &
                "    return" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  valid = .true." // new_line('a') // &  ! Should be REACHABLE when x >= 0
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
        
        cfg = build_control_flow_graph(arena, func_index)
        unreachable_stmts = get_unreachable_statements(cfg)
        
        if (allocated(unreachable_stmts) .and. size(unreachable_stmts) > 0) then
            print *, "FAILED: Code after conditional block incorrectly marked as unreachable"
            print *, "  Number of unreachable statements:", size(unreachable_stmts)
            all_tests_passed = .false.
        else
            print *, "PASSED: Code after conditional block correctly marked as reachable"
        end if
        
    end subroutine test_early_return_in_single_if

    subroutine test_early_return_with_else_branch()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_statements_module, only: parse_function_definition
        use cfg_builder_module, only: build_control_flow_graph
        use control_flow_graph_module, only: control_flow_graph_t, get_unreachable_statements
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(control_flow_graph_t) :: cfg
        integer :: func_index
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Testing early return with else branch..."
        
        source = "function check_value(x) result(status)" // new_line('a') // &
                "  integer :: x, status" // new_line('a') // &
                "  if (x < 0) then" // new_line('a') // &
                "    status = -1" // new_line('a') // &
                "    return" // new_line('a') // &
                "  else" // new_line('a') // &
                "    status = 0" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  status = status + 1" // new_line('a') // &  ! Should be REACHABLE via else branch
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
        
        cfg = build_control_flow_graph(arena, func_index)
        unreachable_stmts = get_unreachable_statements(cfg)
        
        if (allocated(unreachable_stmts) .and. size(unreachable_stmts) > 0) then
            print *, "FAILED: Code after if-else with return incorrectly marked as unreachable"
            print *, "  Number of unreachable statements:", size(unreachable_stmts)
            all_tests_passed = .false.
        else
            print *, "PASSED: Code after if-else with return correctly marked as reachable"
        end if
        
    end subroutine test_early_return_with_else_branch

    subroutine test_early_return_nested_if()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_statements_module, only: parse_function_definition
        use cfg_builder_module, only: build_control_flow_graph
        use control_flow_graph_module, only: control_flow_graph_t, get_unreachable_statements
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(control_flow_graph_t) :: cfg
        integer :: func_index
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Testing early return in nested if..."
        
        source = "function nested_check(x, y) result(res)" // new_line('a') // &
                "  integer :: x, y, res" // new_line('a') // &
                "  if (x < 0) then" // new_line('a') // &
                "    if (y < 0) then" // new_line('a') // &
                "      res = -1" // new_line('a') // &
                "      return" // new_line('a') // &
                "    end if" // new_line('a') // &
                "    res = 0" // new_line('a') // &  ! Reachable when x < 0 and y >= 0
                "    return" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  res = 1" // new_line('a') // &  ! Reachable when x >= 0
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
        
        cfg = build_control_flow_graph(arena, func_index)
        unreachable_stmts = get_unreachable_statements(cfg)
        
        if (allocated(unreachable_stmts) .and. size(unreachable_stmts) > 0) then
            print *, "FAILED: Code after nested if with returns incorrectly marked as unreachable"
            print *, "  Number of unreachable statements:", size(unreachable_stmts)
            all_tests_passed = .false.
        else
            print *, "PASSED: Code after nested if with returns correctly marked as reachable"
        end if
        
    end subroutine test_early_return_nested_if

    subroutine test_early_return_after_if_else()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_statements_module, only: parse_function_definition
        use cfg_builder_module, only: build_control_flow_graph
        use control_flow_graph_module, only: control_flow_graph_t, get_unreachable_statements
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(control_flow_graph_t) :: cfg
        integer :: func_index
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Testing early return in both if and else branches..."
        
        source = "function both_branches_return(x) result(res)" // new_line('a') // &
                "  integer :: x, res" // new_line('a') // &
                "  if (x < 0) then" // new_line('a') // &
                "    res = -1" // new_line('a') // &
                "    return" // new_line('a') // &
                "  else" // new_line('a') // &
                "    res = 1" // new_line('a') // &
                "    return" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  res = 0" // new_line('a') // &  ! Should be UNREACHABLE - both branches return
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
        
        cfg = build_control_flow_graph(arena, func_index)
        unreachable_stmts = get_unreachable_statements(cfg)
        
        ! In this case, code after the if-else SHOULD be unreachable
        if (.not. allocated(unreachable_stmts) .or. size(unreachable_stmts) == 0) then
            print *, "FAILED: Code after if-else where both branches return not marked as unreachable"
            all_tests_passed = .false.
        else
            print *, "PASSED: Code after if-else where both branches return correctly marked as unreachable"
        end if
        
    end subroutine test_early_return_after_if_else

end program test_issue_149_comprehensive