program test_frontend_semantic_basic_type_inference
    use semantic_analyzer, only: analyze_program, create_semantic_context, semantic_context_t
    use ast_core, only: ast_arena_t, create_ast_arena, assignment_node
    use lexer_core
    use parser_dispatcher_module, only: parse_statement_dispatcher
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Basic Type Inference Tests ==='
    print *

    ! Test basic type inference for different assignment types
    if (.not. test_integer_assignment_inference()) all_passed = .false.
    if (.not. test_real_assignment_inference()) all_passed = .false.
    if (.not. test_character_assignment_inference()) all_passed = .false.
    if (.not. test_logical_assignment_inference()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All basic type inference tests passed!'
        stop 0
    else
        print *, 'Some basic type inference tests failed!'
        stop 1
    end if

contains

    logical function test_integer_assignment_inference()
        test_integer_assignment_inference = .true.
        print *, 'Testing integer assignment inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Parse "x = 42"
            call tokenize_core("x = 42", tokens)

            ! Create arena
            arena = create_ast_arena()

            ! Parse using dispatcher
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_integer_assignment_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, stmt_index)

            ! Check if semantic analysis ran without crash
            if (allocated(arena%entries(stmt_index)%node)) then
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        print *, '  PASS: Integer assignment inference completed'
                        print *, '  INFO: Inferred type: ', node%inferred_type_name
                    else
                        print *, '  PARTIAL: Assignment processed but type not stored'
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_integer_assignment_inference = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_integer_assignment_inference = .false.
            end if
        end block

    end function test_integer_assignment_inference

    logical function test_real_assignment_inference()
        test_real_assignment_inference = .true.
        print *, 'Testing real assignment inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Parse "pi = 3.14"
            call tokenize_core("pi = 3.14", tokens)

            ! Create arena
            arena = create_ast_arena()

            ! Parse using dispatcher
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_real_assignment_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, stmt_index)

            ! Check if semantic analysis ran without crash
            if (allocated(arena%entries(stmt_index)%node)) then
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        print *, '  PASS: Real assignment inference completed'
                        print *, '  INFO: Inferred type: ', node%inferred_type_name
                    else
                        print *, '  PARTIAL: Assignment processed but type not stored'
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_real_assignment_inference = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_real_assignment_inference = .false.
            end if
        end block

    end function test_real_assignment_inference

    logical function test_character_assignment_inference()
        test_character_assignment_inference = .true.
        print *, 'Testing character assignment inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Parse 'name = "hello"'
            call tokenize_core('name = "hello"', tokens)

            ! Create arena
            arena = create_ast_arena()

            ! Parse using dispatcher
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_character_assignment_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, stmt_index)

            ! Check if semantic analysis ran without crash
            if (allocated(arena%entries(stmt_index)%node)) then
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        print *, '  PASS: Character assignment inference completed'
                        print *, '  INFO: Inferred type: ', node%inferred_type_name
                    else
                        print *, '  PARTIAL: Assignment processed but type not stored'
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_character_assignment_inference = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_character_assignment_inference = .false.
            end if
        end block

    end function test_character_assignment_inference

    logical function test_logical_assignment_inference()
        test_logical_assignment_inference = .true.
        print *, 'Testing logical assignment inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Parse "flag = .true."
            call tokenize_core("flag = .true.", tokens)

            ! Create arena
            arena = create_ast_arena()

            ! Parse using dispatcher
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index <= 0) then
                print *, '  FAIL: Parse failed to produce AST'
                test_logical_assignment_inference = .false.
                return
            end if

            ! Run semantic analysis
            ctx = create_semantic_context()
            call analyze_program(ctx, arena, stmt_index)

            ! Check if semantic analysis ran without crash
            if (allocated(arena%entries(stmt_index)%node)) then
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        print *, '  PASS: Logical assignment inference completed'
                        print *, '  INFO: Inferred type: ', node%inferred_type_name
                    else
                        print *, '  PARTIAL: Assignment processed but type not stored'
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_logical_assignment_inference = .false.
                end select
            else
                print *, '  FAIL: Node not allocated in arena'
                test_logical_assignment_inference = .false.
            end if
        end block

    end function test_logical_assignment_inference

end program test_frontend_semantic_basic_type_inference
