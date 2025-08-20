program test_parser_logical_relational_operators
    use ast_core, only: ast_arena_t, create_ast_arena
    use parser_test_utilities
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.

    print *, '=== Parser Logical and Relational Operators Tests ==='
    print *

    ! Initialize arena
    arena = create_ast_arena()

    ! Run logical and relational operator tests
    if (.not. test_logical_operators()) all_passed = .false.
    if (.not. test_relational_operators()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All parser logical and relational operator tests passed!'
        stop 0
    else
        print *, 'Some parser logical and relational operator tests failed!'
        stop 1
    end if

contains

    logical function test_logical_operators()
        test_logical_operators = .true.
        print *, 'Test: Logical operators'
        
        ! Test AND
        if (.not. test_binary_expr("a .and. b", ".and.", "a", "b", arena)) then
            test_logical_operators = .false.
        end if
        
        ! Test OR
        if (.not. test_binary_expr("x .or. y", ".or.", "x", "y", arena)) then
            test_logical_operators = .false.
        end if
        
        ! Test EQV 
        if (.not. test_binary_expr("a .eqv. b", ".eqv.", "a", "b", arena)) then
            test_logical_operators = .false.
        end if
        
        ! Test NEQV 
        if (.not. test_binary_expr("a .neqv. b", ".neqv.", "a", "b", arena)) then
            test_logical_operators = .false.
        end if
        
        ! Test complex logical expressions
        if (.not. test_binary_expr("flag1 .and. flag2", ".and.", &
                                   "flag1", "flag2", arena)) then
            test_logical_operators = .false.
        end if
        
        if (.not. test_binary_expr("condition1 .or. condition2", ".or.", &
                                   "condition1", "condition2", arena)) then
            test_logical_operators = .false.
        end if
        
        if (test_logical_operators) then
            print *, 'PASS: Logical operators'
        end if
        
    end function test_logical_operators

    logical function test_relational_operators()
        test_relational_operators = .true.
        print *, 'Test: Relational operators'
        
        ! Test less than
        if (.not. test_binary_expr("a < b", "<", "a", "b", arena)) then
            test_relational_operators = .false.
        end if
        
        ! Test less than or equal
        if (.not. test_binary_expr("x <= y", "<=", "x", "y", arena)) then
            test_relational_operators = .false.
        end if
        
        ! Test greater than
        if (.not. test_binary_expr("m > n", ">", "m", "n", arena)) then
            test_relational_operators = .false.
        end if
        
        ! Test greater than or equal
        if (.not. test_binary_expr("p >= q", ">=", "p", "q", arena)) then
            test_relational_operators = .false.
        end if
        
        ! Test equality
        if (.not. test_binary_expr("i == j", "==", "i", "j", arena)) then
            test_relational_operators = .false.
        end if
        
        ! Test inequality
        if (.not. test_binary_expr("k /= l", "/=", "k", "l", arena)) then
            test_relational_operators = .false.
        end if
        
        ! Test complex relational expressions
        if (.not. test_binary_expr("value1 < value2", "<", &
                                   "value1", "value2", arena)) then
            test_relational_operators = .false.
        end if
        
        if (.not. test_binary_expr("count >= threshold", ">=", &
                                   "count", "threshold", arena)) then
            test_relational_operators = .false.
        end if
        
        if (.not. test_binary_expr("status == SUCCESS", "==", &
                                   "status", "SUCCESS", arena)) then
            test_relational_operators = .false.
        end if
        
        if (test_relational_operators) then
            print *, 'PASS: Relational operators'
        end if
        
    end function test_relational_operators

end program test_parser_logical_relational_operators