program test_logical_operators
    use parser_test_utilities
    use ast_core, only: ast_arena_t, create_ast_arena
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Logical Operators Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_logical_operators_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All logical operator tests passed!'
        stop 0
    else
        print *, 'Some logical operator tests failed!'
        stop 1
    end if

contains

    logical function run_logical_operators_tests()
        run_logical_operators_tests = .true.
        print *, 'Test 1: Logical operators'
        
        ! Test AND
        if (.not. test_binary_expr("a .and. b", ".and.", "a", "b", arena)) then
            run_logical_operators_tests = .false.
        end if
        
        ! Test OR
        if (.not. test_binary_expr("x .or. y", ".or.", "x", "y", arena)) then
            run_logical_operators_tests = .false.
        end if
        
        ! Test EQV 
        if (.not. test_binary_expr("a .eqv. b", ".eqv.", "a", "b", arena)) then
            run_logical_operators_tests = .false.
        end if
        
        ! Test NEQV 
        if (.not. test_binary_expr("a .neqv. b", ".neqv.", "a", "b", arena)) then
            run_logical_operators_tests = .false.
        end if
        
        if (run_logical_operators_tests) then
            print *, 'PASS: Logical operators'
        end if
        
    end function run_logical_operators_tests

end program test_logical_operators