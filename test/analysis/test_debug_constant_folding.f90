program test_debug_constant_folding
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    call test_debug_false_literal()

contains

    subroutine test_debug_false_literal()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_statements_module, only: parse_program_statement
        use cfg_builder_module, only: build_control_flow_graph
        use control_flow_graph_module, only: control_flow_graph_t, print_cfg
        use constant_folding_module, only: evaluate_constant_condition
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(control_flow_graph_t) :: cfg
        integer :: program_index
        integer :: constant_result
        
        print *, "Debug: Testing constant folding with .false. literal..."
        
        source = "program test_debug" // new_line('a') // &
                "  if (.false.) then" // new_line('a') // &
                "    print *, 'dead'" // new_line('a') // &
                "  end if" // new_line('a') // &
                "end program test_debug"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        program_index = parse_program_statement(parser, arena)
        
        if (program_index <= 0) then
            print *, "FAILED: Could not parse program"
            return
        end if
        
        print *, "Program parsed successfully, index =", program_index
        
        ! Let's manually test the constant evaluation
        print *, "AST has", arena%size, "nodes"
        
        ! List all node types
        block
            integer :: i
            print *, "All node types in AST:"
            do i = 1, arena%size
                print *, "  Node", i, "type:", trim(arena%entries(i)%node_type)
            end do
        end block
        
        ! Find the if statement
        block
            integer :: i, if_index, condition_index
            if_index = 0
            
            ! Try both node types
            do i = 1, arena%size
                if (arena%entries(i)%node_type == "if_node" .or. &
                    arena%entries(i)%node_type == "if_statement") then
                    if_index = i
                    print *, "Found if node at index", i, "type:", trim(arena%entries(i)%node_type)
                    exit
                end if
            end do
            
            if (if_index > 0) then
                select type (node => arena%entries(if_index)%node)
                type is (if_node)
                    condition_index = node%condition_index
                    print *, "Condition index:", condition_index
                    
                    ! Test constant evaluation
                    constant_result = evaluate_constant_condition(arena, condition_index)
                    print *, "Constant result:", constant_result
                    print *, "  (0=UNKNOWN, 1=TRUE, 2=FALSE)"
                    
                    if (condition_index > 0 .and. condition_index <= arena%size) then
                        print *, "Condition node type:", arena%entries(condition_index)%node_type
                        select type (cond_node => arena%entries(condition_index)%node)
                        type is (literal_node)
                            print *, "Literal type:", cond_node%literal_type
                            print *, "Literal value:", cond_node%value
                        class default
                            print *, "Condition is not a literal_node"
                        end select
                    end if
                class default
                    print *, "ERROR: Node at if_index is not if_node"
                end select
            else
                print *, "ERROR: No if_node found in AST"
            end if
        end block
        
        ! Build CFG
        cfg = build_control_flow_graph(arena, program_index)
        print *, "CFG built, printing structure:"
        call print_cfg(cfg)
        
    end subroutine test_debug_false_literal

end program test_debug_constant_folding