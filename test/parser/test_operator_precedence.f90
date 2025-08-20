program test_operator_precedence
    use parser_test_utilities
    use ast_core, only: ast_arena_t, create_ast_arena
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Operator Precedence Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_operator_precedence_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All operator precedence tests passed!'
        stop 0
    else
        print *, 'Some operator precedence tests failed!'
        stop 1
    end if

contains

    logical function run_operator_precedence_tests()
        integer :: expr_index
        
        run_operator_precedence_tests = .true.
        print *, 'Test 1: Operator precedence'
        
        ! Test: a + b * c (multiplication should bind tighter)
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("a + b * c", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "a + b * c"'
                run_operator_precedence_tests = .false.
            else
                ! Verify structure: root should be +, right child should be *
                if (.not. verify_precedence_structure(expr_index, "+", "*", arena)) then
                    print *, '  FAIL: Incorrect precedence for "a + b * c"'
                    run_operator_precedence_tests = .false.
                else
                    print *, '  OK: "a + b * c" has correct precedence'
                end if
            end if
        end block
        
        ! Test: a * b + c (multiplication still binds tighter)
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("a * b + c", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "a * b + c"'
                run_operator_precedence_tests = .false.
            else
                ! Verify structure: root should be +, left child should be *
                if (.not. verify_precedence_structure_left(expr_index, "+", "*", arena)) then
                    print *, '  FAIL: Incorrect precedence for "a * b + c"'
                    run_operator_precedence_tests = .false.
                else
                    print *, '  OK: "a * b + c" has correct precedence'
                end if
            end if
        end block
        
        if (run_operator_precedence_tests) then
            print *, 'PASS: Operator precedence'
        end if
        
    end function run_operator_precedence_tests

end program test_operator_precedence