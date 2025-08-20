program test_arithmetic_operators
    use parser_test_utilities
    use ast_core, only: ast_arena_t, create_ast_arena
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Arithmetic Operators Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_arithmetic_operators_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All arithmetic operator tests passed!'
        stop 0
    else
        print *, 'Some arithmetic operator tests failed!'
        stop 1
    end if

contains

    logical function run_arithmetic_operators_tests()
        run_arithmetic_operators_tests = .true.
        print *, 'Test 1: Arithmetic operators'
        
        ! Test addition
        if (.not. test_binary_expr("a + b", "+", "a", "b", arena)) then
            run_arithmetic_operators_tests = .false.
        end if
        
        ! Test subtraction
        if (.not. test_binary_expr("x - y", "-", "x", "y", arena)) then
            run_arithmetic_operators_tests = .false.
        end if
        
        ! Test multiplication
        if (.not. test_binary_expr("m * n", "*", "m", "n", arena)) then
            run_arithmetic_operators_tests = .false.
        end if
        
        ! Test division
        if (.not. test_binary_expr("p / q", "/", "p", "q", arena)) then
            run_arithmetic_operators_tests = .false.
        end if
        
        ! Test exponentiation
        if (.not. test_binary_expr("a ** 2", "**", "a", "2", arena)) then
            run_arithmetic_operators_tests = .false.
        end if
        
        if (run_arithmetic_operators_tests) then
            print *, 'PASS: Arithmetic operators'
        end if
        
    end function run_arithmetic_operators_tests

end program test_arithmetic_operators