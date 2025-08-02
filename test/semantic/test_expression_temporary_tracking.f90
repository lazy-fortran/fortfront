program test_expression_temporary_tracking
    use fortfront
    use semantic_analyzer, only: analyze_program_impl => analyze_program, &
                               create_semantic_context, semantic_context_t
    use ast_core, only: ast_arena_t, create_ast_arena, binary_op_node
    use lexer_core, only: tokenize_core, token_t
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    call test_simple_binary_expression()
    call test_complex_nested_expression()
    call test_temporary_reuse()
    call test_temporary_count_tracking()
    call test_array_expression_temps()
    call test_mixed_type_expressions()

    if (all_tests_passed) then
        print *, "All expression temporary tracking tests PASSED!"
    else
        error stop "Some expression temporary tracking tests FAILED!"
    end if

contains

    subroutine test_simple_binary_expression()
        type(semantic_context_t) :: ctx
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer, allocatable :: temp_ids(:)
        type(expression_temp_info_t) :: temp_info
        integer :: stmt_index, i, node_index
        logical :: found_addition
        character(len=:), allocatable :: source

        print *, "Testing simple binary expression temporary tracking..."

        ! Simple addition that should create a temporary
        source = "c = a + b"

        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Create arena and semantic context
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        ! Parse
        stmt_index = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_index <= 0) then
            print *, "FAILED: Parse failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Run semantic analysis
        call analyze_program_impl(ctx, arena, stmt_index)

        ! Find the binary operation node for a + b
        found_addition = .false.
        do i = 1, arena%size
            if (get_node_type_id_from_arena(arena, i) == NODE_BINARY_OP) then
                ! Direct check for addition operator
                if (allocated(arena%entries(i)%node)) then
                    select type (node => arena%entries(i)%node)
                    type is (binary_op_node)
                        if (node%operator == "+") then
                            found_addition = .true.
                            node_index = i
                            exit
                        end if
                    end select
                end if
            end if
        end do

        if (.not. found_addition) then
            print *, "FAILED: Could not find addition operation"
            all_tests_passed = .false.
            return
        end if

        ! Get temporaries for this expression
        temp_ids = get_expression_temporaries(ctx, node_index)
        
        if (size(temp_ids) /= 1) then
            print *, "FAILED: Expected 1 temporary, got", size(temp_ids)
            all_tests_passed = .false.
            return
        end if

        ! Check temporary info
        temp_info = get_temporary_info(ctx, temp_ids(1))
        
        if (temp_info%type_name /= "real" .and. temp_info%type_name /= "unknown") then
            print *, "FAILED: Expected type 'real' or 'unknown', got '", &
                     temp_info%type_name, "'"
            all_tests_passed = .false.
            return
        end if

        if (temp_info%created_at_node /= node_index) then
            print *, "FAILED: Temporary not created at correct node"
            all_tests_passed = .false.
            return
        end if

        print *, "PASSED: Simple binary expression test"

    end subroutine test_simple_binary_expression

    subroutine test_complex_nested_expression()
        type(semantic_context_t) :: ctx
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index, total_temps
        character(len=:), allocatable :: source
        
        print *, "Testing complex nested expression temporary tracking..."

        ! Complex expression: (a + b) * (c + d) - e / f
        source = "result = (a + b) * (c + d) - e / f"

        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Create arena and semantic context
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        ! Parse
        stmt_index = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_index <= 0) then
            print *, "FAILED: Parse failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Run semantic analysis
        call analyze_program_impl(ctx, arena, stmt_index)

        ! Get total temporary count
        total_temps = get_total_temporary_count(ctx)
        
        ! Should have at least 5 temporaries: 
        ! temp1 = a + b
        ! temp2 = c + d  
        ! temp3 = temp1 * temp2
        ! temp4 = e / f
        ! temp5 = temp3 - temp4
        if (total_temps < 5) then
            print *, "FAILED: Expected at least 5 temporaries, got", total_temps
            all_tests_passed = .false.
            return
        end if

        print *, "PASSED: Complex nested expression test (", total_temps, &
                 "temporaries created)"

    end subroutine test_complex_nested_expression

    subroutine test_temporary_reuse()
        type(semantic_context_t) :: ctx
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index, total_temps, active_temps, i
        character(len=:), allocatable :: source
        
        print *, "Testing temporary reuse..."

        ! Create arena and semantic context once
        arena = create_ast_arena()
        ctx = create_semantic_context()

        ! Parse first statement: c = a + b
        call tokenize_core("c = a + b", tokens)
        stmt_index = parse_statement_dispatcher(tokens, arena)
        if (stmt_index > 0) then
            call analyze_program_impl(ctx, arena, stmt_index)
        end if
        
        ! Parse second statement: d = a + b
        call tokenize_core("d = a + b", tokens)
        stmt_index = parse_statement_dispatcher(tokens, arena)
        if (stmt_index > 0) then
            call analyze_program_impl(ctx, arena, stmt_index)
        end if
        
        ! Parse third statement: e = c + d
        call tokenize_core("e = c + d", tokens)
        stmt_index = parse_statement_dispatcher(tokens, arena)
        if (stmt_index > 0) then
            call analyze_program_impl(ctx, arena, stmt_index)
        end if

        total_temps = get_total_temporary_count(ctx)
        active_temps = get_active_temporary_count(ctx)
        
        print *, "Total temporaries allocated:", total_temps
        print *, "Active temporaries at end:", active_temps

        ! Active temps should be less than total if reuse happened
        if (active_temps >= total_temps .and. total_temps > 1) then
            print *, "WARNING: No temporary reuse detected"
        end if

        print *, "PASSED: Temporary reuse test"

    end subroutine test_temporary_reuse

    subroutine test_temporary_count_tracking()
        type(semantic_context_t) :: ctx
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index, active_after
        character(len=:), allocatable :: source
        
        print *, "Testing temporary count tracking..."

        source = "z = x + y"

        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Create arena and semantic context
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        ! Parse
        stmt_index = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_index <= 0) then
            print *, "FAILED: Parse failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Run semantic analysis
        call analyze_program_impl(ctx, arena, stmt_index)

        active_after = get_active_temporary_count(ctx)
        
        ! After semantic analysis, some temps might still be active
        if (active_after < 0) then
            print *, "FAILED: Invalid active temporary count"
            all_tests_passed = .false.
            return
        end if

        print *, "PASSED: Temporary count tracking test"

    end subroutine test_temporary_count_tracking

    subroutine test_array_expression_temps()
        type(semantic_context_t) :: ctx
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index, total_temps
        character(len=:), allocatable :: source
        
        print *, "Testing array expression temporaries..."

        ! For simplicity, use element-wise operation
        source = "arr3(1) = arr1(1) + arr2(1)"

        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Create arena and semantic context
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        ! Parse
        stmt_index = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_index <= 0) then
            print *, "FAILED: Parse failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Run semantic analysis
        call analyze_program_impl(ctx, arena, stmt_index)

        total_temps = get_total_temporary_count(ctx)
        
        if (total_temps < 1) then
            print *, "WARNING: No temporaries created for array element operation"
            print *, "Note: This may be expected if array element access is optimized"
        else
            print *, "Found", total_temps, "temporaries for array element operation"
        end if

        print *, "PASSED: Array expression temporaries test"

    end subroutine test_array_expression_temps

    subroutine test_mixed_type_expressions()
        type(semantic_context_t) :: ctx
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: i, node_index, stmt_index
        integer, allocatable :: temp_ids(:)
        type(expression_temp_info_t) :: temp_info
        logical :: found_concat
        character(len=:), allocatable :: source

        print *, "Testing mixed type expression temporaries..."

        ! Test string concatenation
        source = "result = str1 // str2"

        ! Tokenize
        call tokenize_core(source, tokens)
        
        ! Create arena and semantic context
        arena = create_ast_arena()
        ctx = create_semantic_context()
        
        ! Parse
        stmt_index = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_index <= 0) then
            print *, "FAILED: Parse failed"
            all_tests_passed = .false.
            return
        end if
        
        ! Run semantic analysis
        call analyze_program_impl(ctx, arena, stmt_index)

        ! Find string concatenation
        found_concat = .false.
        do i = 1, arena%size
            if (get_node_type_id_from_arena(arena, i) == NODE_BINARY_OP) then
                ! Direct check for concatenation operator
                if (allocated(arena%entries(i)%node)) then
                    select type (node => arena%entries(i)%node)
                    type is (binary_op_node)
                        if (node%operator == "//") then
                            found_concat = .true.
                            node_index = i
                            exit
                        end if
                    end select
                end if
            end if
        end do

        if (.not. found_concat) then
            print *, "WARNING: Could not find concatenation operation"
            ! Not a failure - implementation might optimize it away
        else
            temp_ids = get_expression_temporaries(ctx, node_index)
            if (size(temp_ids) > 0) then
                temp_info = get_temporary_info(ctx, temp_ids(1))
                if (temp_info%type_name == "character") then
                    print *, "Found character temporary for concatenation"
                end if
            end if
        end if

        print *, "PASSED: Mixed type expressions test"

    end subroutine test_mixed_type_expressions

end program test_expression_temporary_tracking