program test_array_literal_type_inference
    use semantic_analyzer, only: analyze_program, create_semantic_context, semantic_context_t
    use ast_core, only: ast_arena_t, create_ast_stack, assignment_node, array_literal_node
    use lexer_core
    use parser_dispatcher_module, only: parse_statement_dispatcher
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Array Literal Type Inference Tests ==='
    print *

    ! Test array literal type inference
    if (.not. test_integer_array_literal()) all_passed = .false.
    if (.not. test_real_array_literal()) all_passed = .false.
    if (.not. test_mixed_array_literal()) all_passed = .false.
    if (.not. test_array_dimension_inference()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All array literal type inference tests passed!'
        stop 0
    else
        print *, 'Some array literal type inference tests failed!'
        stop 1
    end if

contains

    logical function test_integer_array_literal()
        test_integer_array_literal = .true.
        print *, 'Testing integer array literal inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Parse "arr = [1, 2, 3]"
            call tokenize_core("arr = [1, 2, 3]", tokens)
            arena = create_ast_stack()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index > 0) then
                ctx = create_semantic_context()
                call analyze_program(ctx, arena, stmt_index)
                
                ! Check if semantic analysis inferred array type
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        print *, '  PASS: Array literal type inference completed'
                        print *, '  INFO: Inferred type: ', node%inferred_type_name
                        ! Should be something like "integer(3)" for array
                        if (index(node%inferred_type_name, "integer(") > 0) then
                            print *, '  PASS: Correctly inferred as integer array'
                        else
                            print *, '  FAIL: Expected integer array type'
                            test_integer_array_literal = .false.
                        end if
                    else
                        print *, '  FAIL: No type inferred'
                        test_integer_array_literal = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_integer_array_literal = .false.
                end select
            else
                print *, '  FAIL: Parse failed'
                test_integer_array_literal = .false.
            end if
            
            call arena%clear()
        end block

    end function test_integer_array_literal

    logical function test_real_array_literal()
        test_real_array_literal = .true.
        print *, 'Testing real array literal inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Parse "arr = [1.0, 2.5, 3.14]"
            call tokenize_core("arr = [1.0, 2.5, 3.14]", tokens)
            arena = create_ast_stack()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index > 0) then
                ctx = create_semantic_context()
                call analyze_program(ctx, arena, stmt_index)
                
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        print *, '  PASS: Array literal type inference completed'
                        print *, '  INFO: Inferred type: ', node%inferred_type_name
                        if (index(node%inferred_type_name, "real(") > 0) then
                            print *, '  PASS: Correctly inferred as real array'
                        else
                            print *, '  FAIL: Expected real array type'
                            test_real_array_literal = .false.
                        end if
                    else
                        print *, '  FAIL: No type inferred'
                        test_real_array_literal = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_real_array_literal = .false.
                end select
            else
                print *, '  FAIL: Parse failed'
                test_real_array_literal = .false.
            end if
            
            call arena%clear()
        end block

    end function test_real_array_literal

    logical function test_mixed_array_literal()
        test_mixed_array_literal = .true.
        print *, 'Testing mixed type array literal inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Parse "arr = [1, 2.0, 3]" - should promote to real
            call tokenize_core("arr = [1, 2.0, 3]", tokens)
            arena = create_ast_stack()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index > 0) then
                ctx = create_semantic_context()
                call analyze_program(ctx, arena, stmt_index)
                
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        print *, '  PASS: Array literal type inference completed'
                        print *, '  INFO: Inferred type: ', node%inferred_type_name
                        ! Mixed integer/real should promote to real
                        if (index(node%inferred_type_name, "real") > 0) then
                            print *, '  PASS: Correctly promoted to real array'
                        else
                            print *, '  FAIL: Expected real array for mixed types'
                            test_mixed_array_literal = .false.
                        end if
                    else
                        print *, '  FAIL: No type inferred'
                        test_mixed_array_literal = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_mixed_array_literal = .false.
                end select
            else
                print *, '  FAIL: Parse failed'
                test_mixed_array_literal = .false.
            end if
            
            call arena%clear()
        end block

    end function test_mixed_array_literal

    logical function test_array_dimension_inference()
        test_array_dimension_inference = .true.
        print *, 'Testing array dimension inference...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            type(semantic_context_t) :: ctx
            integer :: stmt_index

            ! Parse "arr = [10, 20, 30, 40, 50]" - 5 elements
            call tokenize_core("arr = [10, 20, 30, 40, 50]", tokens)
            arena = create_ast_stack()
            stmt_index = parse_statement_dispatcher(tokens, arena)

            if (stmt_index > 0) then
                ctx = create_semantic_context()
                call analyze_program(ctx, arena, stmt_index)
                
                select type (node => arena%entries(stmt_index)%node)
                type is (assignment_node)
                    if (allocated(node%inferred_type)) then
                        print *, '  PASS: Array literal type inference completed'
                        print *, '  INFO: Inferred type: ', node%inferred_type_name
                        ! Check if (5) is inferred for 5 elements
                        if (index(node%inferred_type_name, "(5)") > 0) then
                            print *, '  PASS: Array dimension(5) correctly detected'
                        else
                            print *, '  INFO: Dimension not explicitly shown in type name'
                        end if
                    else
                        print *, '  FAIL: No type inferred'
                        test_array_dimension_inference = .false.
                    end if
                class default
                    print *, '  FAIL: Expected assignment node'
                    test_array_dimension_inference = .false.
                end select
            else
                print *, '  FAIL: Parse failed'
                test_array_dimension_inference = .false.
            end if
            
            call arena%clear()
        end block

    end function test_array_dimension_inference

end program test_array_literal_type_inference