program test_issue_146_constant_folding
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    call test_impossible_false_condition()
    call test_always_true_condition()
    call test_constant_expression_folding()

    if (all_tests_passed) then
        print *, "All issue #146 tests PASSED!"
    else
        print *, "Some issue #146 tests FAILED!"
        stop 1
    end if

contains

    subroutine test_impossible_false_condition()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_execution_statements_module, only: parse_program_statement
        use cfg_builder_module, only: build_control_flow_graph
        use control_flow_graph_module, only: control_flow_graph_t, get_unreachable_statements
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(control_flow_graph_t) :: cfg
        integer :: program_index
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Testing impossible false condition (.false.)..."
        
        ! Test case from issue #146
        source = "program test_impossible_if" // new_line('a') // &
                "  if (.false.) then" // new_line('a') // &
                "    print *, 'this is dead code'" // new_line('a') // &
                "    call some_subroutine()" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  print *, 'this is reachable'" // new_line('a') // &
                "end program test_impossible_if"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        program_index = parse_program_statement(parser, arena)
        
        if (program_index <= 0) then
            print *, "FAILED: Could not parse program"
            all_tests_passed = .false.
            return
        end if
        
        ! Build control flow graph
        cfg = build_control_flow_graph(arena, program_index)
        
        ! Find unreachable statements
        unreachable_stmts = get_unreachable_statements(cfg)
        
        ! Should find statements in the if block as unreachable due to constant folding
        if (.not. allocated(unreachable_stmts) .or. size(unreachable_stmts) == 0) then
            print *, "FAILED: Dead code in impossible if condition not detected"
            print *, "Note: This may indicate constant folding is not implemented yet"
            all_tests_passed = .false.
        else
            print *, "PASSED: Dead code in impossible if condition correctly detected"
        end if
        
    end subroutine test_impossible_false_condition

    subroutine test_always_true_condition()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_execution_statements_module, only: parse_program_statement
        use cfg_builder_module, only: build_control_flow_graph
        use control_flow_graph_module, only: control_flow_graph_t, get_unreachable_statements
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(control_flow_graph_t) :: cfg
        integer :: program_index
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Testing always true condition (.true.)..."
        
        source = "program test_always_true" // new_line('a') // &
                "  if (.true.) then" // new_line('a') // &
                "    print *, 'this is reachable'" // new_line('a') // &
                "  else" // new_line('a') // &
                "    print *, 'this is dead code'" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  print *, 'this is also reachable'" // new_line('a') // &
                "end program test_always_true"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        program_index = parse_program_statement(parser, arena)
        
        if (program_index <= 0) then
            print *, "FAILED: Could not parse program"
            all_tests_passed = .false.
            return
        end if
        
        cfg = build_control_flow_graph(arena, program_index)
        unreachable_stmts = get_unreachable_statements(cfg)
        
        ! Should find statements in the else block as unreachable due to constant folding
        if (.not. allocated(unreachable_stmts) .or. size(unreachable_stmts) == 0) then
            print *, "FAILED: Dead code in always-true else branch not detected"
            print *, "Note: This may indicate constant folding is not implemented yet"
            all_tests_passed = .false.
        else
            print *, "PASSED: Dead code in always-true else branch correctly detected"
        end if
        
    end subroutine test_always_true_condition

    subroutine test_constant_expression_folding()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_execution_statements_module, only: parse_program_statement
        use cfg_builder_module, only: build_control_flow_graph
        use control_flow_graph_module, only: control_flow_graph_t, get_unreachable_statements
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(control_flow_graph_t) :: cfg
        integer :: program_index
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Testing constant expression folding (1 == 0)..."
        
        source = "program test_constant_expr" // new_line('a') // &
                "  if (1 == 0) then" // new_line('a') // &
                "    print *, 'impossible condition'" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  if (2 > 1) then" // new_line('a') // &
                "    print *, 'always true - this is reachable'" // new_line('a') // &
                "  else" // new_line('a') // &
                "    print *, 'always false - dead code'" // new_line('a') // &
                "  end if" // new_line('a') // &
                "end program test_constant_expr"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        program_index = parse_program_statement(parser, arena)
        
        if (program_index <= 0) then
            print *, "FAILED: Could not parse program"
            all_tests_passed = .false.
            return
        end if
        
        cfg = build_control_flow_graph(arena, program_index)
        unreachable_stmts = get_unreachable_statements(cfg)
        
        ! Should find dead code in both impossible conditions
        if (.not. allocated(unreachable_stmts) .or. size(unreachable_stmts) == 0) then
            print *, "FAILED: Dead code in constant expressions not detected"
            print *, "Note: This may indicate constant expression folding is not implemented yet"
            all_tests_passed = .false.
        else
            print *, "PASSED: Dead code in constant expressions correctly detected"
            print *, "Found", size(unreachable_stmts), "unreachable statements"
        end if
        
    end subroutine test_constant_expression_folding

end program test_issue_146_constant_folding