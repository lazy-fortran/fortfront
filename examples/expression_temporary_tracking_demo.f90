program expression_temporary_tracking_demo
    ! Demonstrates the expression temporary tracking feature in fortfront
    ! This program shows how to track temporary variables created during
    ! expression evaluation for optimization purposes
    
    use fortfront
    use semantic_analyzer, only: analyze_program_impl => analyze_program, &
                               create_semantic_context, semantic_context_t
    use ast_core, only: ast_arena_t, create_ast_arena, binary_op_node
    use lexer_core, only: tokenize_core, token_t
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use iso_fortran_env, only: error_unit, output_unit
    implicit none

    call demonstrate_temporary_tracking()

contains

    subroutine demonstrate_temporary_tracking()
        type(semantic_context_t) :: ctx
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index, i, j
        integer, allocatable :: temp_ids(:)
        type(expression_temp_info_t) :: temp_info
        character(len=:), allocatable :: source
        
        print *, "=== Expression Temporary Tracking Demo ==="
        print *
        
        ! Example: Complex nested expression
        source = "result = (a + b) * (c - d) + e / f"
        print *, "Analyzing expression: ", source
        print *
        
        ! Tokenize and parse
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        ctx = create_semantic_context()
        stmt_index = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_index <= 0) then
            write(error_unit, '(A)') "Error: Failed to parse expression"
            write(error_unit, '(A,A)') "Input: ", source
            return
        end if
        
        ! Run semantic analysis to generate temporary tracking
        block
            use iso_fortran_env, only: error_unit
            
            ! Protect against potential semantic analysis errors
            call analyze_program_impl(ctx, arena, stmt_index)
            
            ! Check if semantic analysis succeeded (basic check)
            if (.not. allocated(arena%entries)) then
                write(error_unit, '(A)') &
                    "Warning: Semantic analysis may have failed"
            end if
        end block
        
        ! Display temporary statistics
        print *, "Temporary Variable Statistics:"
        print *, "------------------------------"
        print *, "Total temporaries allocated:", get_total_temporary_count(ctx)
        print *, "Currently active temporaries:", get_active_temporary_count(ctx)
        print *
        
        ! Show details for each expression node that uses temporaries
        print *, "Expression Node Temporary Usage:"
        print *, "--------------------------------"
        
        do i = 1, arena%size
            if (get_node_type_id_from_arena(arena, i) == NODE_BINARY_OP) then
                temp_ids = get_expression_temporaries(ctx, i)
                
                if (size(temp_ids) > 0) then
                    ! Get operator info
                    if (allocated(arena%entries(i)%node)) then
                        select type (node => arena%entries(i)%node)
                        type is (binary_op_node)
                            write (output_unit, '(A,I3,A,A,A,I2,A)') &
                                "Node ", i, " (", trim(node%operator), &
                                ") uses ", size(temp_ids), " temporary(s):"
                            
                            ! Show details for each temporary
                            do j = 1, size(temp_ids)
                                temp_info = get_temporary_info(ctx, temp_ids(j))
                                write (output_unit, '(A,I3,A,A,A,I3,A)') &
                                    "  - Temp ", temp_info%temp_id, &
                                    " (", trim(temp_info%type_name), &
                                    ", ", temp_info%size_bytes, " bytes)"
                            end do
                        end select
                    end if
                end if
            end if
        end do
        
        print *
        print *, "Temporary Reuse Analysis:"
        print *, "-------------------------"
        
        ! Analyze multiple statements to show reuse
        call analyze_reuse_pattern()
        
    end subroutine demonstrate_temporary_tracking
    
    subroutine analyze_reuse_pattern()
        type(semantic_context_t) :: ctx
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index
        integer :: temps_before, temps_after
        
        ! Create fresh context
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        print *, "Analyzing temporary reuse across multiple statements..."
        print *
        
        ! Statement 1
        call tokenize_core("x = a + b", tokens)
        stmt_index = parse_statement_dispatcher(tokens, arena)
        if (stmt_index > 0) then
            call analyze_program_impl(ctx, arena, stmt_index)
        else
            write(error_unit, '(A)') "Warning: Failed to parse statement 1"
        end if
        temps_before = get_total_temporary_count(ctx)
        print *, "After statement 1: ", temps_before, " temporaries allocated"
        
        ! Statement 2 (similar type, could reuse)
        call tokenize_core("y = c + d", tokens)
        stmt_index = parse_statement_dispatcher(tokens, arena)
        if (stmt_index > 0) then
            call analyze_program_impl(ctx, arena, stmt_index)
        else
            write(error_unit, '(A)') "Warning: Failed to parse statement 2"
        end if
        temps_after = get_total_temporary_count(ctx)
        
        if (temps_after > temps_before) then
            print *, "After statement 2: ", temps_after, &
                     " temporaries (new allocation)"
        else
            print *, "After statement 2: ", temps_after, &
                     " temporaries (reused existing)"
        end if
        
        ! Statement 3
        call tokenize_core("z = x * y", tokens)
        stmt_index = parse_statement_dispatcher(tokens, arena)
        if (stmt_index > 0) then
            call analyze_program_impl(ctx, arena, stmt_index)
        else
            write(error_unit, '(A)') "Warning: Failed to parse statement 3"
        end if
        print *, "After statement 3: ", get_total_temporary_count(ctx), &
                 " temporaries total"
        print *, "Currently active: ", get_active_temporary_count(ctx)
        
        print *
        print *, "This demonstrates how the temporary tracker can help"
        print *, "identify opportunities for memory optimization."
        
    end subroutine analyze_reuse_pattern

end program expression_temporary_tracking_demo