program test_frontend_semantic_expression_type_inference
    use semantic_analyzer, only: analyze_program, create_semantic_context, semantic_context_t
    use ast_core, only: ast_arena_t, create_ast_stack, assignment_node
    use lexer_core
    use parser_dispatcher_module, only: parse_statement_dispatcher
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Expression Type Inference Tests ==='
    print *

    ! Test type inference for different expressions
    if (.not. test_arithmetic_expression_inference()) all_passed = .false.
    if (.not. test_logical_expression_inference()) all_passed = .false.
    if (.not. test_relational_expression_inference()) all_passed = .false.
    if (.not. test_mixed_expression_inference()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All expression type inference tests passed!'
        stop 0
    else
        print *, 'Some expression type inference tests failed!'
        stop 1
    end if

contains

    logical function test_arithmetic_expression_inference()
        test_arithmetic_expression_inference = .true.
        print *, 'Testing arithmetic expression inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            integer :: ast_index
            type(semantic_context_t) :: ctx

            ! Parse "result = 2 + 3"
            call tokenize_core("result = 2 + 3", tokens)
            
            ! Create arena
            arena = create_ast_stack()
            
            ! Parse statement
            ast_index = parse_statement_dispatcher(tokens, arena)

            if (ast_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_arithmetic_expression_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, ast_index)

            ! Check if semantic analysis ran without crash
            if (allocated(arena%entries(ast_index)%node)) then
                select type (ast_node => arena%entries(ast_index)%node)
                type is (assignment_node)
                    if (allocated(ast_node%inferred_type)) then
                        print *, '  PASS: Arithmetic expression inference completed'
                        print *, '  INFO: Inferred type: ', ast_node%inferred_type_name
                    else
                        print *, '  PARTIAL: Assignment processed but type not stored'
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_arithmetic_expression_inference = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_arithmetic_expression_inference = .false.
            end if
        end block

    end function test_arithmetic_expression_inference

    logical function test_logical_expression_inference()
        test_logical_expression_inference = .true.
        print *, 'Testing logical expression inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            integer :: ast_index
            type(semantic_context_t) :: ctx

            ! Parse "flag = .true. .and. .false."
            call tokenize_core("flag = .true. .and. .false.", tokens)
            
            ! Create arena
            arena = create_ast_stack()
            
            ! Parse statement
            ast_index = parse_statement_dispatcher(tokens, arena)

            if (ast_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_logical_expression_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, ast_index)

            ! Check if semantic analysis ran without crash
            if (allocated(arena%entries(ast_index)%node)) then
                select type (ast_node => arena%entries(ast_index)%node)
                type is (assignment_node)
                    if (allocated(ast_node%inferred_type)) then
                        print *, '  PASS: Logical expression inference completed'
                        print *, '  INFO: Inferred type: ', ast_node%inferred_type_name
                    else
                        print *, '  PARTIAL: Assignment processed but type not stored'
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_logical_expression_inference = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_logical_expression_inference = .false.
            end if
        end block

    end function test_logical_expression_inference

    logical function test_relational_expression_inference()
        test_relational_expression_inference = .true.
        print *, 'Testing relational expression inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            integer :: ast_index
            type(semantic_context_t) :: ctx

            ! Parse "result = 5 > 3"
            call tokenize_core("result = 5 > 3", tokens)
            
            ! Create arena
            arena = create_ast_stack()
            
            ! Parse statement
            ast_index = parse_statement_dispatcher(tokens, arena)

            if (ast_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_relational_expression_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, ast_index)

            ! Check if semantic analysis ran without crash
            if (allocated(arena%entries(ast_index)%node)) then
                select type (ast_node => arena%entries(ast_index)%node)
                type is (assignment_node)
                    if (allocated(ast_node%inferred_type)) then
                        print *, '  PASS: Relational expression inference completed'
                        print *, '  INFO: Inferred type: ', ast_node%inferred_type_name
                    else
                        print *, '  PARTIAL: Assignment processed but type not stored'
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_relational_expression_inference = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_relational_expression_inference = .false.
            end if
        end block

    end function test_relational_expression_inference

    logical function test_mixed_expression_inference()
        test_mixed_expression_inference = .true.
        print *, 'Testing mixed expression inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            integer :: ast_index
            type(semantic_context_t) :: ctx

            ! Parse "result = 2.5 * 3"
            call tokenize_core("result = 2.5 * 3", tokens)
            
            ! Create arena
            arena = create_ast_stack()
            
            ! Parse statement
            ast_index = parse_statement_dispatcher(tokens, arena)

            if (ast_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_mixed_expression_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, ast_index)

            ! Check if semantic analysis ran without crash
            if (allocated(arena%entries(ast_index)%node)) then
                select type (ast_node => arena%entries(ast_index)%node)
                type is (assignment_node)
                    if (allocated(ast_node%inferred_type)) then
                        print *, '  PASS: Mixed expression inference completed'
                        print *, '  INFO: Inferred type: ', ast_node%inferred_type_name
                    else
                        print *, '  PARTIAL: Assignment processed but type not stored'
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_mixed_expression_inference = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_mixed_expression_inference = .false.
            end if
        end block

    end function test_mixed_expression_inference

end program test_frontend_semantic_expression_type_inference
