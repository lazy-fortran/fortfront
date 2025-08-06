program test_conditional_return_flow
    use iso_fortran_env, only: error_unit
    use lexer_core, only: tokenize_core, token_t
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_statements_module, only: parse_function_definition
    use ast_core, only: ast_arena_t, create_ast_arena
    use cfg_builder_module, only: cfg_builder_t, create_cfg_builder, build_control_flow_graph
    use control_flow_graph_module, only: control_flow_graph_t, find_unreachable_code, &
                                         get_unreachable_statements, print_cfg
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    call test_conditional_return_reachability()
    call test_unconditional_return_unreachability()
    call test_nested_conditional_returns()
    call test_multiple_conditional_returns()

    if (all_tests_passed) then
        print *, "All conditional return flow tests PASSED!"
    else
        error stop "Some conditional return flow tests FAILED!"
    end if

contains

    subroutine test_conditional_return_reachability()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: func_index
        type(cfg_builder_t) :: builder
        type(control_flow_graph_t) :: cfg
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Testing code after conditional return is reachable..."
        
        ! Test case from issue #137
        source = "function validate(x) result(valid)" // new_line('a') // &
                "  integer :: x" // new_line('a') // &
                "  logical :: valid" // new_line('a') // &
                "  if (x < 0) then" // new_line('a') // &
                "    valid = .false." // new_line('a') // &
                "    return" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  valid = .true." // new_line('a') // &  ! This should be REACHABLE
                "end function"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        func_index = parse_function_definition(parser, arena)
        
        if (func_index <= 0) then
            write(error_unit, *) "FAILED: Could not parse function"
            all_tests_passed = .false.
            return
        end if
        
        ! Build control flow graph
        builder = create_cfg_builder()
        cfg = build_control_flow_graph(arena, func_index)
        
        ! Check for unreachable statements
        unreachable_stmts = get_unreachable_statements(cfg)
        
        ! The assignment "valid = .true." after the if block should NOT be unreachable
        if (allocated(unreachable_stmts) .and. size(unreachable_stmts) > 0) then
            write(error_unit, *) "FAILED: Code after conditional return incorrectly marked as unreachable"
            write(error_unit, *) "  Number of unreachable statements:", size(unreachable_stmts)
            all_tests_passed = .false.
        else
            print *, "PASSED: Code after conditional return is correctly marked as reachable"
        end if
        
    end subroutine test_conditional_return_reachability

    subroutine test_unconditional_return_unreachability()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: func_index
        type(cfg_builder_t) :: builder
        type(control_flow_graph_t) :: cfg
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Testing code after unconditional return is unreachable..."
        
        source = "function always_false() result(res)" // new_line('a') // &
                "  logical :: res" // new_line('a') // &
                "  res = .false." // new_line('a') // &
                "  return" // new_line('a') // &
                "  res = .true." // new_line('a') // &  ! This SHOULD be unreachable
                "end function"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        func_index = parse_function_definition(parser, arena)
        
        if (func_index <= 0) then
            write(error_unit, *) "FAILED: Could not parse function"
            all_tests_passed = .false.
            return
        end if
        
        ! Build control flow graph
        builder = create_cfg_builder()
        cfg = build_control_flow_graph(arena, func_index)
        
        ! Check for unreachable statements
        unreachable_stmts = get_unreachable_statements(cfg)
        
        ! The assignment "res = .true." after unconditional return SHOULD be unreachable
        if (.not. allocated(unreachable_stmts) .or. size(unreachable_stmts) == 0) then
            write(error_unit, *) "FAILED: Code after unconditional return not marked as unreachable"
            all_tests_passed = .false.
        else
            print *, "PASSED: Code after unconditional return correctly marked as unreachable"
        end if
        
    end subroutine test_unconditional_return_unreachability

    subroutine test_nested_conditional_returns()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: func_index
        type(cfg_builder_t) :: builder
        type(control_flow_graph_t) :: cfg
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Testing nested conditional returns..."
        
        source = "function process(x, y) result(res)" // new_line('a') // &
                "  integer :: x, y, res" // new_line('a') // &
                "  if (x < 0) then" // new_line('a') // &
                "    if (y < 0) then" // new_line('a') // &
                "      res = -1" // new_line('a') // &
                "      return" // new_line('a') // &
                "    end if" // new_line('a') // &
                "    res = 0" // new_line('a') // &  ! Reachable when y >= 0
                "  end if" // new_line('a') // &
                "  res = 1" // new_line('a') // &  ! Reachable when x >= 0
                "end function"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        func_index = parse_function_definition(parser, arena)
        
        if (func_index <= 0) then
            write(error_unit, *) "FAILED: Could not parse function"
            all_tests_passed = .false.
            return
        end if
        
        ! Build control flow graph
        builder = create_cfg_builder()
        cfg = build_control_flow_graph(arena, func_index)
        
        ! Check for unreachable statements
        unreachable_stmts = get_unreachable_statements(cfg)
        
        ! Both "res = 0" and "res = 1" should be reachable
        if (allocated(unreachable_stmts) .and. size(unreachable_stmts) > 0) then
            write(error_unit, *) "FAILED: Code after nested conditional returns incorrectly marked as unreachable"
            write(error_unit, *) "  Number of unreachable statements:", size(unreachable_stmts)
            all_tests_passed = .false.
        else
            print *, "PASSED: Code after nested conditional returns correctly marked as reachable"
        end if
        
    end subroutine test_nested_conditional_returns

    subroutine test_multiple_conditional_returns()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: func_index
        type(cfg_builder_t) :: builder
        type(control_flow_graph_t) :: cfg
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Testing multiple conditional returns..."
        
        source = "function classify(x) result(category)" // new_line('a') // &
                "  integer :: x, category" // new_line('a') // &
                "  if (x < 0) then" // new_line('a') // &
                "    category = -1" // new_line('a') // &
                "    return" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  if (x == 0) then" // new_line('a') // &
                "    category = 0" // new_line('a') // &
                "    return" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  category = 1" // new_line('a') // &  ! Reachable when x > 0
                "end function"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        func_index = parse_function_definition(parser, arena)
        
        if (func_index <= 0) then
            write(error_unit, *) "FAILED: Could not parse function"
            all_tests_passed = .false.
            return
        end if
        
        ! Build control flow graph
        builder = create_cfg_builder()
        cfg = build_control_flow_graph(arena, func_index)
        
        ! Check for unreachable statements
        unreachable_stmts = get_unreachable_statements(cfg)
        
        
        ! "category = 1" should be reachable
        if (allocated(unreachable_stmts) .and. size(unreachable_stmts) > 0) then
            write(error_unit, *) "FAILED: Code after multiple conditional returns incorrectly marked as unreachable"
            write(error_unit, *) "  Number of unreachable statements:", size(unreachable_stmts)
            write(error_unit, *) "  Unreachable statement indices:", unreachable_stmts
            all_tests_passed = .false.
        else
            print *, "PASSED: Code after multiple conditional returns correctly marked as reachable"
        end if
        
    end subroutine test_multiple_conditional_returns

end program test_conditional_return_flow