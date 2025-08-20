program test_complex_expressions
    use ast_core, only: ast_arena_t, create_ast_arena
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Complex Expressions Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_complex_expressions_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All complex expression tests passed!'
        stop 0
    else
        print *, 'Some complex expression tests failed!'
        stop 1
    end if

contains

    logical function run_complex_expressions_tests()
        integer :: expr_index
        
        run_complex_expressions_tests = .true.
        print *, 'Test 1: Complex expressions'
        
        ! Test arithmetic with parentheses
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("a + b * (c - d) / e", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse complex arithmetic'
                run_complex_expressions_tests = .false.
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
                run_complex_expressions_tests = .false.
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
                run_complex_expressions_tests = .false.
            else
                print *, '  OK: Expression with functions parsed'
            end if
        end block
        
        if (run_complex_expressions_tests) then
            print *, 'PASS: Complex expressions'
        end if
        
    end function run_complex_expressions_tests

end program test_complex_expressions