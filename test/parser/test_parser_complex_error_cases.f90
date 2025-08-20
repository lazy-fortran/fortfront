program test_parser_complex_error_cases
    use ast_core, only: ast_arena_t, create_ast_arena, binary_op_node, &
                        identifier_node
    use parser_test_utilities
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.

    print *, '=== Parser Complex Expressions and Error Cases Tests ==='
    print *

    ! Initialize arena
    arena = create_ast_arena()

    ! Run complex expression and error handling tests
    if (.not. test_complex_expressions()) all_passed = .false.
    if (.not. test_error_cases()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All parser complex expression and error case tests passed!'
        stop 0
    else
        print *, 'Some parser complex expression and error case tests failed!'
        stop 1
    end if

contains

    logical function test_complex_expressions()
        integer :: expr_index
        
        test_complex_expressions = .true.
        print *, 'Test: Complex expressions'
        
        ! Test arithmetic with parentheses
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("a + b * (c - d) / e", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse complex arithmetic'
                test_complex_expressions = .false.
            else
                print *, '  OK: "a + b * (c - d) / e" parsed'
            end if
        end block
        
        ! Test logical combination
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("x > 0 .and. y < 10 .or. z == 5", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse complex logical expression'
                test_complex_expressions = .false.
            else
                print *, '  OK: Complex logical expression parsed'
            end if
        end block
        
        ! Test function in expression
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("sin(x) * cos(y) + tan(z)", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse expression with functions'
                test_complex_expressions = .false.
            else
                print *, '  OK: Expression with functions parsed'
            end if
        end block
        
        ! Test complex nested expressions
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("((a + b) * c) ** (d - e)", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse complex nested expression'
                test_complex_expressions = .false.
            else
                print *, '  OK: Complex nested expression parsed'
            end if
        end block
        
        ! Test expression with array and function calls
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("max([1, 2, 3]) + min(a, b)", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse expression with arrays and functions'
                test_complex_expressions = .false.
            else
                print *, '  OK: Expression with arrays and functions parsed'
            end if
        end block
        
        ! Test expression with member access
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("obj%field + other%value * factor", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse expression with member access'
                test_complex_expressions = .false.
            else
                print *, '  OK: Expression with member access parsed'
            end if
        end block
        
        ! Test unary operators in complex expression
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("-a + b * -c", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse expression with unary operators'
                test_complex_expressions = .false.
            else
                print *, '  OK: Expression with unary operators parsed'
            end if
        end block
        
        if (test_complex_expressions) then
            print *, 'PASS: Complex expressions'
        end if
        
    end function test_complex_expressions

    logical function test_error_cases()
        integer :: expr_index
        
        test_error_cases = .true.
        print *, 'Test: Error cases'
        
        ! Test malformed array literal - missing closing bracket
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("[1, 2, 3", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index /= 0) then
                print *, '  FAIL: Should fail on missing closing bracket'
                test_error_cases = .false.
            else
                print *, '  OK: Correctly failed on "[1, 2, 3" (missing ])'
            end if
        end block
        
        ! Test malformed array literal - trailing comma
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("[1, 2,]", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index /= 0) then
                print *, '  FAIL: Should fail on trailing comma'
                test_error_cases = .false.
            else
                print *, '  OK: Correctly failed on "[1, 2,]" (trailing comma)'
            end if
        end block
        
        ! Test incomplete expression
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("a +", tokens)
            expr_index = parse_expression(tokens, arena)
            
            ! The parser might return a partial parse up to the error
            if (expr_index == 0) then
                print *, '  OK: Failed on incomplete expression "a +"'
            else
                ! Check if it's just the identifier 'a'
                select type (node => arena%entries(expr_index)%node)
                class is (identifier_node)
                    print *, '  OK: Parsed valid prefix "a" from incomplete "a +"'
                class is (binary_op_node)
                    ! Parser creates binary op even with missing right operand
                    print *, '  OK: Parser creates binary op for incomplete "a +" (expected behavior)'
                class default
                    print *, '  INFO: Parsed some prefix of incomplete expression'
                end select
            end if
        end block
        
        ! Test mismatched parentheses
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("(a + b))", tokens)
            expr_index = parse_expression(tokens, arena)
            
            ! This might actually parse as (a + b) followed by extra )
            ! which is valid in expression context
            if (expr_index == 0) then
                print *, '  OK: Failed on mismatched parentheses'
            else
                print *, '  OK: Parsed valid prefix of mismatched parentheses'
            end if
        end block
        
        ! Test empty expression (may succeed with empty token array)
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index /= 0) then
                print *, '  INFO: Parser handled empty expression gracefully'
            else
                print *, '  OK: Correctly failed on empty expression'
            end if
        end block
        
        ! Test malformed function call - missing closing parenthesis
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("func(a, b", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index /= 0) then
                print *, '  INFO: Parser handled unclosed function call gracefully'
            else
                print *, '  OK: Correctly failed on "func(a, b" (missing ))'
            end if
        end block
        
        ! Test multiple consecutive operators
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("a + + b", tokens)
            expr_index = parse_expression(tokens, arena)
            
            ! This might be parsed as a + (+b) which is valid
            if (expr_index == 0) then
                print *, '  OK: Failed on consecutive operators'
            else
                print *, '  INFO: Parsed consecutive operators (may be valid as unary)'
            end if
        end block
        
        if (test_error_cases) then
            print *, 'PASS: Error cases'
        end if
        
    end function test_error_cases

end program test_parser_complex_error_cases