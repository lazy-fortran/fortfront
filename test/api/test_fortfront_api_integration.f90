program test_fortfront_api_integration
    ! Integration test for the complete fortfront public API pipeline
    use fortfront
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== fortfront Public API Integration Tests ==='
    print *
    
    ! Test complete pipeline integration
    if (.not. test_complete_pipeline()) all_passed = .false.
    if (.not. test_arena_navigation_pipeline()) all_passed = .false.
    if (.not. test_type_system_integration()) all_passed = .false.
    if (.not. test_visitor_pattern_integration()) all_passed = .false.
    
    ! Report results
    print *
    if (all_passed) then
        print *, 'All fortfront API integration tests passed!'
        stop 0
    else
        print *, 'Some fortfront API integration tests failed!'
        stop 1
    end if
    
contains
    
    logical function test_complete_pipeline()
        test_complete_pipeline = .true.
        print *, 'Testing complete pipeline integration...'
        
        block
            character(len=:), allocatable :: source, error_msg, fortran_code
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: prog_index
            
            ! Complex source code using multiple features
            source = '! Test program with multiple features' // new_line('A') // &
                     'real function compute(x, y)' // new_line('A') // &
                     '    real :: x, y' // new_line('A') // &
                     '    compute = x * x + y * y' // new_line('A') // &
                     'end function compute' // new_line('A') // &
                     '' // new_line('A') // &
                     '! Main computation' // new_line('A') // &
                     'a := 3.0' // new_line('A') // &
                     'b := 4.0' // new_line('A') // &
                     'result = compute(a, b)' // new_line('A') // &
                     'print *, "Result:", result'
            
            ! Phase 1: Lexical analysis
            call lex_source(source, tokens, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Lexer error: ', error_msg
                test_complete_pipeline = .false.
                return
            end if
            
            if (.not. allocated(tokens) .or. size(tokens) < 20) then
                print *, '  FAIL: Insufficient tokens generated'
                test_complete_pipeline = .false.
                return
            end if
            
            ! Phase 2: Parsing
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            if (error_msg /= "") then
                print *, '  FAIL: Parser error: ', error_msg
                test_complete_pipeline = .false.
                return
            end if
            
            if (prog_index <= 0) then
                print *, '  FAIL: Invalid program index'
                test_complete_pipeline = .false.
                return
            end if
            
            ! Phase 3: Semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, prog_index)
            
            ! Verify type information was added
            block
                class(ast_node), allocatable :: node
                type(mono_type_t), allocatable :: node_type
                
                node = get_node(arena, prog_index)
                if (allocated(node)) then
                    select type (node)
                    type is (program_node)
                        if (size(node%body_indices) > 0) then
                            ! Check if types were inferred
                            block
                                logical :: type_found
                                call get_type_for_node(arena, node%body_indices(1), node_type, type_found)
                                if (type_found .and. allocated(node_type)) then
                                    print *, '  Note: Type inference successful'
                                end if
                            end block
                        end if
                    end select
                end if
            end block
            
            ! Phase 4: Code generation
            call emit_fortran(arena, prog_index, fortran_code)
            if (.not. allocated(fortran_code)) then
                print *, '  FAIL: No code generated'
                test_complete_pipeline = .false.
                return
            end if
            
            ! Verify generated code contains expected elements
            if (index(fortran_code, 'real function compute') == 0) then
                print *, '  FAIL: Missing function in generated code'
                test_complete_pipeline = .false.
                return
            end if
            
            if (index(fortran_code, 'real :: a') == 0) then
                print *, '  FAIL: Missing inferred type declaration'
                test_complete_pipeline = .false.
                return
            end if
            
            print *, '  PASS: Complete pipeline integration'
            print *, '  Generated code length:', len_trim(fortran_code), 'characters'
        end block
    end function test_complete_pipeline
    
    logical function test_arena_navigation_pipeline()
        test_arena_navigation_pipeline = .true.
        print *, 'Testing arena navigation in pipeline...'
        
        block
            character(len=:), allocatable :: source, error_msg
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(ast_arena_stats_t) :: stats
            integer :: prog_index
            integer, allocatable :: children(:)
            
            source = 'x = 1' // new_line('A') // &
                     'y = 2' // new_line('A') // &
                     'z = x + y'
            
            ! Process through pipeline
            call lex_source(source, tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            call analyze_semantics(arena, prog_index)
            
            ! Test arena navigation
            stats = get_arena_stats(arena)
            if (stats%total_nodes < 5) then
                print *, '  FAIL: Expected more nodes in arena'
                test_arena_navigation_pipeline = .false.
                return
            end if
            
            ! Navigate through AST
            block
                class(ast_node), allocatable :: prog_node, stmt_node
                integer :: parent_idx
                
                prog_node = get_node(arena, prog_index)
                if (.not. allocated(prog_node)) then
                    print *, '  FAIL: Could not get program node'
                    test_arena_navigation_pipeline = .false.
                    return
                end if
                
                select type (prog_node)
                type is (program_node)
                    if (size(prog_node%body_indices) >= 3) then
                        ! Get last statement
                        stmt_node = get_node(arena, prog_node%body_indices(3))
                        if (allocated(stmt_node)) then
                            select type (stmt_node)
                            type is (assignment_node)
                                ! Navigate to value expression
                                if (stmt_node%value_index > 0) then
                                    block
                                        class(ast_node), allocatable :: expr_node
                                        expr_node = get_node(arena, stmt_node%value_index)
                                        if (allocated(expr_node)) then
                                            select type (expr_node)
                                            type is (binary_op_node)
                                                if (expr_node%operator /= '+') then
                                                    print *, '  FAIL: Expected + operator'
                                                    test_arena_navigation_pipeline = .false.
                                                    return
                                                end if
                                            end select
                                        end if
                                    end block
                                end if
                            end select
                        end if
                    end if
                end select
            end block
            
            print *, '  PASS: Arena navigation in pipeline'
        end block
    end function test_arena_navigation_pipeline
    
    logical function test_type_system_integration()
        test_type_system_integration = .true.
        print *, 'Testing type system integration...'
        
        block
            character(len=:), allocatable :: source, error_msg, code
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            integer :: prog_index
            
            ! Mixed type operations
            source = 'x := 42' // new_line('A') // &
                     'y := 3.14' // new_line('A') // &
                     'z := x + y' // new_line('A') // &
                     'arr := [1, 2, 3]' // new_line('A') // &
                     's := "hello"'
            
            ! Process through pipeline
            call lex_source(source, tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            call analyze_semantics(arena, prog_index)
            call emit_fortran(arena, prog_index, code)
            
            ! Verify type handling
            if (index(code, 'integer :: x') == 0) then
                print *, '  FAIL: Missing integer type'
                test_type_system_integration = .false.
                return
            end if
            
            if (index(code, 'real') == 0) then
                print *, '  FAIL: Missing real type'
                test_type_system_integration = .false.
                return
            end if
            
            if (index(code, 'character') == 0) then
                print *, '  FAIL: Missing character type'
                test_type_system_integration = .false.
                return
            end if
            
            ! Check type information in AST
            block
                class(ast_node), allocatable :: node
                type(mono_type_t), allocatable :: node_type
                
                node = get_node(arena, prog_index)
                if (allocated(node)) then
                    select type (node)
                    type is (program_node)
                        if (size(node%body_indices) > 0) then
                            ! Get first assignment
                            block
                                class(ast_node), allocatable :: assign_node
                                assign_node = get_node(arena, node%body_indices(1))
                                if (allocated(assign_node)) then
                                    select type (assign_node)
                                    type is (assignment_node)
                                        block
                                            logical :: type_found
                                            call get_type_for_node(arena, assign_node%target_index, node_type, type_found)
                                            if (type_found .and. allocated(node_type)) then
                                                if (node_type%kind /= TINT) then
                                                    print *, '  FAIL: Wrong type for x'
                                                    test_type_system_integration = .false.
                                                    return
                                                end if
                                            end if
                                        end block
                                    end select
                                end if
                            end block
                        end if
                    end select
                end if
            end block
            
            print *, '  PASS: Type system integration'
        end block
    end function test_type_system_integration
    
    logical function test_visitor_pattern_integration()
        test_visitor_pattern_integration = .true.
        print *, 'Testing AST node counting...'
        
        block
            character(len=:), allocatable :: source, error_msg
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            integer :: prog_index
            integer :: assignment_count, identifier_count, literal_count, binary_op_count
            integer :: i
            class(ast_node), allocatable :: node
            
            source = 'do i = 1, 10' // new_line('A') // &
                     '    x(i) = i * i' // new_line('A') // &
                     'end do'
            
            ! Process through pipeline
            call lex_source(source, tokens, error_msg)
            arena = create_ast_arena()
            call parse_tokens(tokens, arena, prog_index, error_msg)
            call analyze_semantics(arena, prog_index)
            
            ! Count node types manually
            assignment_count = 0
            identifier_count = 0
            literal_count = 0
            binary_op_count = 0
            
            ! Traverse all nodes in arena
            do i = 1, arena%size
                node = get_node(arena, i)
                if (allocated(node)) then
                    select type (node)
                    type is (assignment_node)
                        assignment_count = assignment_count + 1
                    type is (identifier_node)
                        identifier_count = identifier_count + 1
                    type is (literal_node)
                        literal_count = literal_count + 1
                    type is (binary_op_node)
                        binary_op_count = binary_op_count + 1
                    end select
                end if
            end do
            
            if (assignment_count == 0) then
                print *, '  FAIL: No assignments found'
                test_visitor_pattern_integration = .false.
                return
            end if
            
            if (binary_op_count == 0) then
                print *, '  FAIL: No binary operations found'
                test_visitor_pattern_integration = .false.
                return
            end if
            
            print *, '  PASS: AST node counting'
            print *, '  Node stats: assignments=', assignment_count, &
                     ', identifiers=', identifier_count, &
                     ', literals=', literal_count, &
                     ', binary_ops=', binary_op_count
        end block
    end function test_visitor_pattern_integration
    
end program test_fortfront_api_integration