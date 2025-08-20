program test_parentheses_expressions
    use ast_core, only: ast_arena_t, create_ast_arena, binary_op_node
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Parentheses Expressions Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_parentheses_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All parentheses expression tests passed!'
        stop 0
    else
        print *, 'Some parentheses expression tests failed!'
        stop 1
    end if

contains

    logical function run_parentheses_tests()
        integer :: expr_index
        
        run_parentheses_tests = .true.
        print *, 'Test 1: Parentheses'
        
        ! Test: (a + b) * c (parentheses override precedence)
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("(a + b) * c", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "(a + b) * c"'
                run_parentheses_tests = .false.
            else
                ! Root should be *, left child should be +
                select type (node => arena%entries(expr_index)%node)
                class is (binary_op_node)
                    if (node%operator /= "*") then
                        print *, '  FAIL: Root should be * for "(a + b) * c"'
                        run_parentheses_tests = .false.
                    else
                        print *, '  OK: "(a + b) * c" parsed with correct structure'
                    end if
                class default
                    print *, '  FAIL: Expected binary op node'
                    run_parentheses_tests = .false.
                end select
            end if
        end block
        
        ! Test nested parentheses
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("((a + b) * (c - d))", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse nested parentheses'
                run_parentheses_tests = .false.
            else
                print *, '  OK: Nested parentheses parsed'
            end if
        end block
        
        if (run_parentheses_tests) then
            print *, 'PASS: Parentheses'
        end if
        
    end function run_parentheses_tests

end program test_parentheses_expressions