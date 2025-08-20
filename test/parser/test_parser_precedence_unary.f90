program test_parser_precedence_unary
    use ast_core, only: ast_arena_t, create_ast_arena, binary_op_node
    use parser_test_utilities
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.

    print *, '=== Parser Precedence and Unary Operators Tests ==='
    print *

    ! Initialize arena
    arena = create_ast_arena()

    ! Run precedence and unary operator tests
    if (.not. test_operator_precedence()) all_passed = .false.
    if (.not. test_parentheses()) all_passed = .false.
    if (.not. test_unary_operators()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All parser precedence and unary operator tests passed!'
        stop 0
    else
        print *, 'Some parser precedence and unary operator tests failed!'
        stop 1
    end if

contains

    logical function test_operator_precedence()
        integer :: expr_index
        
        test_operator_precedence = .true.
        print *, 'Test: Operator precedence'
        
        ! Test: a + b * c (multiplication should bind tighter)
        arena = create_ast_arena()  ! Fresh arena
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("a + b * c", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "a + b * c"'
                test_operator_precedence = .false.
            else
                ! Verify structure: root should be +, right child should be *
                if (.not. verify_precedence_structure(expr_index, "+", "*", &
                                                      arena)) then
                    print *, '  FAIL: Incorrect precedence for "a + b * c"'
                    test_operator_precedence = .false.
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
                test_operator_precedence = .false.
            else
                ! Verify structure: root should be +, left child should be *
                if (.not. verify_precedence_structure_left(expr_index, "+", "*", &
                                                           arena)) then
                    print *, '  FAIL: Incorrect precedence for "a * b + c"'
                    test_operator_precedence = .false.
                else
                    print *, '  OK: "a * b + c" has correct precedence'
                end if
            end if
        end block
        
        ! Test: a ** b * c (exponentiation binds tighter than multiplication)
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("a ** b * c", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "a ** b * c"'
                test_operator_precedence = .false.
            else
                ! Verify structure: root should be *, left child should be **
                if (.not. verify_precedence_structure_left(expr_index, "*", "**", &
                                                           arena)) then
                    print *, '  FAIL: Incorrect precedence for "a ** b * c"'
                    test_operator_precedence = .false.
                else
                    print *, '  OK: "a ** b * c" has correct precedence'
                end if
            end if
        end block
        
        if (test_operator_precedence) then
            print *, 'PASS: Operator precedence'
        end if
        
    end function test_operator_precedence

    logical function test_parentheses()
        integer :: expr_index
        
        test_parentheses = .true.
        print *, 'Test: Parentheses'
        
        ! Test: (a + b) * c (parentheses override precedence)
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("(a + b) * c", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "(a + b) * c"'
                test_parentheses = .false.
            else
                ! Root should be *, left child should be +
                select type (node => arena%entries(expr_index)%node)
                class is (binary_op_node)
                    if (node%operator /= "*") then
                        print *, '  FAIL: Root should be * for "(a + b) * c"'
                        test_parentheses = .false.
                    else
                        print *, '  OK: "(a + b) * c" parsed with correct structure'
                    end if
                class default
                    print *, '  FAIL: Expected binary op node'
                    test_parentheses = .false.
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
                test_parentheses = .false.
            else
                print *, '  OK: Nested parentheses parsed'
            end if
        end block
        
        ! Test: a * (b + c) (parentheses override precedence)
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("a * (b + c)", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "a * (b + c)"'
                test_parentheses = .false.
            else
                ! Root should be *, right child should be +
                if (.not. verify_precedence_structure(expr_index, "*", "+", &
                                                      arena)) then
                    print *, '  FAIL: Incorrect structure for "a * (b + c)"'
                    test_parentheses = .false.
                else
                    print *, '  OK: "a * (b + c)" has correct structure'
                end if
            end if
        end block
        
        if (test_parentheses) then
            print *, 'PASS: Parentheses'
        end if
        
    end function test_parentheses

    logical function test_unary_operators()
        integer :: expr_index
        
        test_unary_operators = .true.
        print *, 'Test: Unary operators'
        
        ! Test unary minus
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("-x", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "-x"'
                test_unary_operators = .false.
            else
                print *, '  OK: "-x" parsed'
            end if
        end block
        
        ! Test unary plus
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("+y", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "+y"'
                test_unary_operators = .false.
            else
                print *, '  OK: "+y" parsed'
            end if
        end block
        
        ! Test logical NOT
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core(".not. flag", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse ".not. flag"'
                test_unary_operators = .false.
            else
                print *, '  OK: ".not. flag" parsed'
            end if
        end block
        
        ! Test unary minus with complex expression
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("-(a + b)", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "-(a + b)"'
                test_unary_operators = .false.
            else
                print *, '  OK: "-(a + b)" parsed'
            end if
        end block
        
        if (test_unary_operators) then
            print *, 'PASS: Unary operators'
        end if
        
    end function test_unary_operators

end program test_parser_precedence_unary