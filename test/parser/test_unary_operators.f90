program test_unary_operators
    use ast_core, only: ast_arena_t, create_ast_arena
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Unary Operators Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_unary_operators_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All unary operator tests passed!'
        stop 0
    else
        print *, 'Some unary operator tests failed!'
        stop 1
    end if

contains

    logical function run_unary_operators_tests()
        integer :: expr_index
        
        run_unary_operators_tests = .true.
        print *, 'Test 1: Unary operators'
        
        ! Test unary minus
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("-x", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "-x"'
                run_unary_operators_tests = .false.
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
                run_unary_operators_tests = .false.
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
                run_unary_operators_tests = .false.
            else
                print *, '  OK: ".not. flag" parsed'
            end if
        end block
        
        if (run_unary_operators_tests) then
            print *, 'PASS: Unary operators'
        end if
        
    end function run_unary_operators_tests

end program test_unary_operators