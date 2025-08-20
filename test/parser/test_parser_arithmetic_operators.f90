program test_parser_arithmetic_operators
    use ast_core, only: ast_arena_t, create_ast_arena
    use parser_test_utilities
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.

    print *, '=== Parser Arithmetic Operators Tests ==='
    print *

    ! Initialize arena
    arena = create_ast_arena()

    ! Run arithmetic operator tests
    if (.not. test_arithmetic_operators()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All parser arithmetic operator tests passed!'
        stop 0
    else
        print *, 'Some parser arithmetic operator tests failed!'
        stop 1
    end if

contains

    logical function test_arithmetic_operators()
        test_arithmetic_operators = .true.
        print *, 'Test: Arithmetic operators'
        
        ! Test addition
        if (.not. test_binary_expr("a + b", "+", "a", "b", arena)) then
            test_arithmetic_operators = .false.
        end if
        
        ! Test subtraction
        if (.not. test_binary_expr("x - y", "-", "x", "y", arena)) then
            test_arithmetic_operators = .false.
        end if
        
        ! Test multiplication
        if (.not. test_binary_expr("m * n", "*", "m", "n", arena)) then
            test_arithmetic_operators = .false.
        end if
        
        ! Test division
        if (.not. test_binary_expr("p / q", "/", "p", "q", arena)) then
            test_arithmetic_operators = .false.
        end if
        
        ! Test exponentiation
        if (.not. test_binary_expr("a ** 2", "**", "a", "2", arena)) then
            test_arithmetic_operators = .false.
        end if
        
        ! Test more complex arithmetic expressions
        if (.not. test_binary_expr("2.5 + 3.7", "+", "2.5", "3.7", arena)) then
            test_arithmetic_operators = .false.
        end if
        
        if (.not. test_binary_expr("100 - 25", "-", "100", "25", arena)) then
            test_arithmetic_operators = .false.
        end if
        
        if (.not. test_binary_expr("var1 * var2", "*", "var1", "var2", arena)) then
            test_arithmetic_operators = .false.
        end if
        
        if (.not. test_binary_expr("numerator / denominator", "/", &
                                   "numerator", "denominator", arena)) then
            test_arithmetic_operators = .false.
        end if
        
        if (.not. test_binary_expr("base ** exponent", "**", &
                                   "base", "exponent", arena)) then
            test_arithmetic_operators = .false.
        end if
        
        if (test_arithmetic_operators) then
            print *, 'PASS: Arithmetic operators'
        end if
        
    end function test_arithmetic_operators

end program test_parser_arithmetic_operators