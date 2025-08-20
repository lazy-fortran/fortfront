program test_relational_operators
    use parser_test_utilities
    use ast_core, only: ast_arena_t, create_ast_arena
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Relational Operators Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_relational_operators_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All relational operator tests passed!'
        stop 0
    else
        print *, 'Some relational operator tests failed!'
        stop 1
    end if

contains

    logical function run_relational_operators_tests()
        run_relational_operators_tests = .true.
        print *, 'Test 1: Relational operators'
        
        ! Test less than
        if (.not. test_binary_expr("a < b", "<", "a", "b", arena)) then
            run_relational_operators_tests = .false.
        end if
        
        ! Test less than or equal
        if (.not. test_binary_expr("x <= y", "<=", "x", "y", arena)) then
            run_relational_operators_tests = .false.
        end if
        
        ! Test greater than
        if (.not. test_binary_expr("m > n", ">", "m", "n", arena)) then
            run_relational_operators_tests = .false.
        end if
        
        ! Test greater than or equal
        if (.not. test_binary_expr("p >= q", ">=", "p", "q", arena)) then
            run_relational_operators_tests = .false.
        end if
        
        ! Test equality
        if (.not. test_binary_expr("i == j", "==", "i", "j", arena)) then
            run_relational_operators_tests = .false.
        end if
        
        ! Test inequality
        if (.not. test_binary_expr("k /= l", "/=", "k", "l", arena)) then
            run_relational_operators_tests = .false.
        end if
        
        if (run_relational_operators_tests) then
            print *, 'PASS: Relational operators'
        end if
        
    end function run_relational_operators_tests

end program test_relational_operators