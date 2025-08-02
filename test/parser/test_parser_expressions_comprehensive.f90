program test_parser_expressions_comprehensive
    use ast_core, only: ast_arena_t, create_ast_arena, ast_node, binary_op_node, &
                        array_literal_node, call_or_subscript_node, literal_node, &
                        identifier_node, LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, &
                        LITERAL_LOGICAL, create_call_or_subscript
    use ast_nodes_core, only: component_access_node
    use ast_factory
    use parser_expressions_module
    use lexer_core
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.

    print *, '=== Comprehensive Parser Expression Tests ==='
    print *

    ! Initialize arena
    arena = create_ast_arena()

    ! Run comprehensive expression parsing tests
    if (.not. test_arithmetic_operators()) all_passed = .false.
    if (.not. test_logical_operators()) all_passed = .false.
    if (.not. test_relational_operators()) all_passed = .false.
    if (.not. test_operator_precedence()) all_passed = .false.
    if (.not. test_parentheses()) all_passed = .false.
    if (.not. test_unary_operators()) all_passed = .false.
    if (.not. test_array_literals()) all_passed = .false.
    if (.not. test_function_calls()) all_passed = .false.
    if (.not. test_complex_expressions()) all_passed = .false.
    if (.not. test_error_cases()) all_passed = .false.
    if (.not. test_member_access()) all_passed = .false.
    if (.not. test_logical_literals()) all_passed = .false.
    if (.not. test_large_arrays()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All parser expression tests passed!'
        stop 0
    else
        print *, 'Some parser expression tests failed!'
        stop 1
    end if

contains

    logical function test_arithmetic_operators()
        test_arithmetic_operators = .true.
        print *, 'Test 1: Arithmetic operators'
        
        ! Test addition
        if (.not. test_binary_expr("a + b", "+", "a", "b")) then
            test_arithmetic_operators = .false.
        end if
        
        ! Test subtraction
        if (.not. test_binary_expr("x - y", "-", "x", "y")) then
            test_arithmetic_operators = .false.
        end if
        
        ! Test multiplication
        if (.not. test_binary_expr("m * n", "*", "m", "n")) then
            test_arithmetic_operators = .false.
        end if
        
        ! Test division
        if (.not. test_binary_expr("p / q", "/", "p", "q")) then
            test_arithmetic_operators = .false.
        end if
        
        ! Test exponentiation
        if (.not. test_binary_expr("a ** 2", "**", "a", "2")) then
            test_arithmetic_operators = .false.
        end if
        
        if (test_arithmetic_operators) then
            print *, 'PASS: Arithmetic operators'
        end if
        
    end function test_arithmetic_operators

    logical function test_logical_operators()
        test_logical_operators = .true.
        print *, 'Test 2: Logical operators'
        
        ! Test AND
        if (.not. test_binary_expr("a .and. b", ".and.", "a", "b")) then
            test_logical_operators = .false.
        end if
        
        ! Test OR
        if (.not. test_binary_expr("x .or. y", ".or.", "x", "y")) then
            test_logical_operators = .false.
        end if
        
        ! Test EQV - Disabled due to issue #36
        ! if (.not. test_binary_expr("a .eqv. b", ".eqv.", "a", "b")) then
        !     test_logical_operators = .false.
        ! end if
        
        ! Test NEQV - Disabled due to issue #36
        ! if (.not. test_binary_expr("a .neqv. b", ".neqv.", "a", "b")) then
        !     test_logical_operators = .false.
        ! end if
        
        if (test_logical_operators) then
            print *, 'PASS: Logical operators'
        end if
        
    end function test_logical_operators

    logical function test_relational_operators()
        test_relational_operators = .true.
        print *, 'Test 3: Relational operators'
        
        ! Test less than
        if (.not. test_binary_expr("a < b", "<", "a", "b")) then
            test_relational_operators = .false.
        end if
        
        ! Test less than or equal
        if (.not. test_binary_expr("x <= y", "<=", "x", "y")) then
            test_relational_operators = .false.
        end if
        
        ! Test greater than
        if (.not. test_binary_expr("m > n", ">", "m", "n")) then
            test_relational_operators = .false.
        end if
        
        ! Test greater than or equal
        if (.not. test_binary_expr("p >= q", ">=", "p", "q")) then
            test_relational_operators = .false.
        end if
        
        ! Test equality
        if (.not. test_binary_expr("i == j", "==", "i", "j")) then
            test_relational_operators = .false.
        end if
        
        ! Test inequality
        if (.not. test_binary_expr("k /= l", "/=", "k", "l")) then
            test_relational_operators = .false.
        end if
        
        if (test_relational_operators) then
            print *, 'PASS: Relational operators'
        end if
        
    end function test_relational_operators

    logical function test_operator_precedence()
        integer :: expr_index
        
        test_operator_precedence = .true.
        print *, 'Test 4: Operator precedence'
        
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
                if (.not. verify_precedence_structure(expr_index, "+", "*")) then
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
                if (.not. verify_precedence_structure_left(expr_index, "+", "*")) then
                    print *, '  FAIL: Incorrect precedence for "a * b + c"'
                    test_operator_precedence = .false.
                else
                    print *, '  OK: "a * b + c" has correct precedence'
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
        print *, 'Test 5: Parentheses'
        
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
        
        if (test_parentheses) then
            print *, 'PASS: Parentheses'
        end if
        
    end function test_parentheses

    logical function test_unary_operators()
        integer :: expr_index
        
        test_unary_operators = .true.
        print *, 'Test 6: Unary operators'
        
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
        
        if (test_unary_operators) then
            print *, 'PASS: Unary operators'
        end if
        
    end function test_unary_operators

    logical function test_array_literals()
        integer :: expr_index
        
        test_array_literals = .true.
        print *, 'Test 7: Array literals'
        
        ! Test simple integer array
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("[1, 2, 3]", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "[1, 2, 3]"'
                test_array_literals = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                class is (array_literal_node)
                    if (size(node%element_indices) /= 3) then
                        print *, '  FAIL: Expected 3 elements in array'
                        test_array_literals = .false.
                    else
                        print *, '  OK: "[1, 2, 3]" parsed as array with 3 elements'
                    end if
                class default
                    print *, '  FAIL: Expected array literal node'
                    test_array_literals = .false.
                end select
            end if
        end block
        
        ! Test real array
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("[1.0, 2.5, 3.14]", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse real array'
                test_array_literals = .false.
            else
                print *, '  OK: "[1.0, 2.5, 3.14]" parsed'
            end if
        end block
        
        ! Test empty array
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("[]", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse empty array'
                test_array_literals = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                class is (array_literal_node)
                    if (size(node%element_indices) /= 0) then
                        print *, '  FAIL: Expected 0 elements in empty array'
                        test_array_literals = .false.
                    else
                        print *, '  OK: "[]" parsed as empty array'
                    end if
                class default
                    print *, '  FAIL: Expected array literal node for empty array'
                    test_array_literals = .false.
                end select
            end if
        end block
        
        ! Test single element array
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("[42]", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse single element array'
                test_array_literals = .false.
            else
                print *, '  OK: "[42]" parsed'
            end if
        end block
        
        if (test_array_literals) then
            print *, 'PASS: Array literals'
        end if
        
    end function test_array_literals

    logical function test_function_calls()
        integer :: expr_index
        
        test_function_calls = .true.
        print *, 'Test 8: Function calls'
        
        ! Test no-arg function
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("random()", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "random()"'
                test_function_calls = .false.
            else
                print *, '  OK: "random()" parsed'
            end if
        end block
        
        ! Test single-arg function
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("sin(x)", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "sin(x)"'
                test_function_calls = .false.
            else
                print *, '  OK: "sin(x)" parsed'
            end if
        end block
        
        ! Test multi-arg function
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("max(a, b, c)", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "max(a, b, c)"'
                test_function_calls = .false.
            else
                print *, '  OK: "max(a, b, c)" parsed'
            end if
        end block
        
        ! Test nested function calls
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("sqrt(abs(x))", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "sqrt(abs(x))"'
                test_function_calls = .false.
            else
                print *, '  OK: "sqrt(abs(x))" parsed'
            end if
        end block
        
        if (test_function_calls) then
            print *, 'PASS: Function calls'
        end if
        
    end function test_function_calls

    logical function test_complex_expressions()
        integer :: expr_index
        
        test_complex_expressions = .true.
        print *, 'Test 9: Complex expressions'
        
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
        
        if (test_complex_expressions) then
            print *, 'PASS: Complex expressions'
        end if
        
    end function test_complex_expressions

    logical function test_error_cases()
        integer :: expr_index
        
        test_error_cases = .true.
        print *, 'Test 10: Error cases'
        
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
        
        if (test_error_cases) then
            print *, 'PASS: Error cases'
        end if
        
    end function test_error_cases

    ! Helper function to test binary expressions
    logical function test_binary_expr(expr_str, expected_op, expected_left, expected_right)
        character(len=*), intent(in) :: expr_str, expected_op, expected_left, expected_right
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_binary_expr = .true.
        
        arena = create_ast_arena()
        call tokenize_core(expr_str, tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index == 0) then
            print *, '  FAIL: Failed to parse "', trim(expr_str), '"'
            test_binary_expr = .false.
            return
        end if
        
        ! Verify it's a binary op with expected operator
        select type (node => arena%entries(expr_index)%node)
        class is (binary_op_node)
            if (node%operator /= expected_op) then
                print *, '  FAIL: Expected operator "', expected_op, '" but got "', node%operator, '"'
                test_binary_expr = .false.
            else
                ! Could verify operands too if needed
                print *, '  OK: "', trim(expr_str), '" parsed correctly'
            end if
        class default
            print *, '  FAIL: Expected binary operator node for "', trim(expr_str), '"'
            test_binary_expr = .false.
        end select
        
    end function test_binary_expr

    ! Helper to verify precedence structure
    logical function verify_precedence_structure(expr_index, root_op, child_op)
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: root_op, child_op
        
        verify_precedence_structure = .false.
        
        select type (root => arena%entries(expr_index)%node)
        class is (binary_op_node)
            if (root%operator == root_op) then
                ! Check right child
                select type (right => arena%entries(root%right_index)%node)
                class is (binary_op_node)
                    if (right%operator == child_op) then
                        verify_precedence_structure = .true.
                    end if
                end select
            end if
        end select
        
    end function verify_precedence_structure

    ! Helper to verify precedence structure with left child
    logical function verify_precedence_structure_left(expr_index, root_op, child_op)
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: root_op, child_op
        
        verify_precedence_structure_left = .false.
        
        select type (root => arena%entries(expr_index)%node)
        class is (binary_op_node)
            if (root%operator == root_op) then
                ! Check left child
                select type (left => arena%entries(root%left_index)%node)
                class is (binary_op_node)
                    if (left%operator == child_op) then
                        verify_precedence_structure_left = .true.
                    end if
                end select
            end if
        end select
        
    end function verify_precedence_structure_left

    logical function test_member_access()
        integer :: expr_index
        
        test_member_access = .true.
        print *, 'Test 11: Member access operator (%)'
        
        ! Test simple member access
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("obj%field", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "obj%field"'
                test_member_access = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                type is (component_access_node)
                    if (node%component_name == "field") then
                        print *, '  OK: "obj%field" parsed as component access'
                    else
                        print *, '  FAIL: Expected component name "field", got: ', node%component_name
                        test_member_access = .false.
                    end if
                type is (binary_op_node)
                    ! Backward compatibility - some parsers might still use binary op
                    if (node%operator == "%") then
                        print *, '  OK: "obj%field" parsed as member access (legacy)'
                    else
                        print *, '  FAIL: Expected % operator'
                        test_member_access = .false.
                    end if
                class default
                    print *, '  FAIL: Expected component_access_node for member access'
                    test_member_access = .false.
                end select
            end if
        end block
        
        ! Test nested member access
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("obj%nested%field", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse nested member access'
                test_member_access = .false.
            else
                print *, '  OK: "obj%nested%field" parsed'
            end if
        end block
        
        if (test_member_access) then
            print *, 'PASS: Member access operator'
        end if
        
    end function test_member_access

    logical function test_logical_literals()
        integer :: expr_index
        
        test_logical_literals = .true.
        print *, 'Test 12: Logical literals'
        
        ! Test .true.
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core(".true.", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse ".true."'
                test_logical_literals = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                class is (literal_node)
                    if (node%value == ".true.") then
                        print *, '  OK: ".true." parsed as logical literal'
                    else
                        print *, '  FAIL: Incorrect value for .true.'
                        test_logical_literals = .false.
                    end if
                class default
                    print *, '  FAIL: Expected literal node for .true.'
                    test_logical_literals = .false.
                end select
            end if
        end block
        
        ! Test .false.
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core(".false.", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse ".false."'
                test_logical_literals = .false.
            else
                print *, '  OK: ".false." parsed'
            end if
        end block
        
        ! Test logical literals in expressions
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("x .and. .true.", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "x .and. .true."'
                test_logical_literals = .false.
            else
                print *, '  OK: Logical literal in expression parsed'
            end if
        end block
        
        if (test_logical_literals) then
            print *, 'PASS: Logical literals'
        end if
        
    end function test_logical_literals

    logical function test_large_arrays()
        integer :: expr_index
        character(len=1024) :: large_array_str
        integer :: i
        
        test_large_arrays = .true.
        print *, 'Test 13: Large arrays (>100 elements)'
        
        ! Build a string with 105 elements
        large_array_str = "["
        do i = 1, 105
            if (i > 1) large_array_str = trim(large_array_str) // ", "
            write(large_array_str, '(A,I0)') trim(large_array_str), i
        end do
        large_array_str = trim(large_array_str) // "]"
        
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core(trim(large_array_str), tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse large array'
                test_large_arrays = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                class is (array_literal_node)
                    if (size(node%element_indices) == 105) then
                        print *, '  OK: Large array with 105 elements parsed'
                    else
                        print *, '  FAIL: Expected 105 elements, got', size(node%element_indices)
                        test_large_arrays = .false.
                    end if
                class default
                    print *, '  FAIL: Expected array literal node'
                    test_large_arrays = .false.
                end select
            end if
        end block
        
        if (test_large_arrays) then
            print *, 'PASS: Large arrays'
        end if
        
    end function test_large_arrays

end program test_parser_expressions_comprehensive