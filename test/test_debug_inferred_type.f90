program test_debug_inferred_type
    use standardizer
    use ast_core
    use lexer_core
    use parser_dispatcher_module
    use semantic_analyzer
    use ast_nodes_core
    implicit none
    
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    type(semantic_context_t) :: ctx
    integer :: prog_index, assign_index
    
    ! Test: Integer array [2, 3, 4]
    print *, "Testing: i = [2, 3, 4]"
    call tokenize_core("i = [2, 3, 4]", tokens)
    arena = create_ast_arena()
    prog_index = parse_statement_dispatcher(tokens, arena)
    
    if (prog_index > 0) then
        ! Analyze semantics to get type information
        ctx = create_semantic_context()
        call analyze_program(ctx, arena, prog_index)
        
        ! Check if the array literal has inferred_type
        select type (node => arena%entries(prog_index)%node)
        type is (assignment_node)
            print *, "Found assignment node"
            if (node%value_index > 0) then
                select type (value => arena%entries(node%value_index)%node)
                type is (array_literal_node)
                    print *, "Found array literal node"
                    if (allocated(value%inferred_type)) then
                        print *, "  inferred_type allocated: YES"
                        print *, "  inferred_type%kind: ", value%inferred_type%kind
                        if (allocated(value%inferred_type%args)) then
                            print *, "  Element type kind: ", value%inferred_type%args(1)%kind
                        end if
                    else
                        print *, "  inferred_type allocated: NO"
                    end if
                end select
            end if
        end select
    end if
    
    call arena%clear()
    
end program test_debug_inferred_type