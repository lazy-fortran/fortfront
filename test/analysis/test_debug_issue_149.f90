program test_debug_issue_149
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    call test_both_branches_return_debug()

contains

    subroutine test_both_branches_return_debug()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_statements_module, only: parse_function_definition
        use cfg_builder_module, only: build_control_flow_graph
        use control_flow_graph_module, only: control_flow_graph_t, get_unreachable_statements, print_cfg
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(control_flow_graph_t) :: cfg
        integer :: func_index
        integer, allocatable :: unreachable_stmts(:)
        
        print *, "Debug: Testing both branches return..."
        
        source = "function both_branches_return(x) result(res)" // new_line('a') // &
                "  integer :: x, res" // new_line('a') // &
                "  if (x < 0) then" // new_line('a') // &
                "    res = -1" // new_line('a') // &
                "    return" // new_line('a') // &
                "  else" // new_line('a') // &
                "    res = 1" // new_line('a') // &
                "    return" // new_line('a') // &
                "  end if" // new_line('a') // &
                "  res = 0" // new_line('a') // &  ! Should be UNREACHABLE
                "end function"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        func_index = parse_function_definition(parser, arena)
        
        if (func_index <= 0) then
            print *, "FAILED: Could not parse function"
            return
        end if
        
        print *, "Function parsed successfully"
        
        cfg = build_control_flow_graph(arena, func_index)
        
        print *, "CFG built, printing structure:"
        call print_cfg(cfg)
        
        unreachable_stmts = get_unreachable_statements(cfg)
        
        if (allocated(unreachable_stmts)) then
            print *, "Number of unreachable statements:", size(unreachable_stmts)
        else
            print *, "No unreachable statements detected"
        end if
        
    end subroutine test_both_branches_return_debug

end program test_debug_issue_149