program test_function_calls
    use ast_core, only: ast_arena_t, create_ast_arena
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Function Calls Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_function_calls_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All function call tests passed!'
        stop 0
    else
        print *, 'Some function call tests failed!'
        stop 1
    end if

contains

    logical function run_function_calls_tests()
        integer :: expr_index
        
        run_function_calls_tests = .true.
        print *, 'Test 1: Function calls'
        
        ! Test no-arg function
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("random()", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "random()"'
                run_function_calls_tests = .false.
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
                run_function_calls_tests = .false.
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
                run_function_calls_tests = .false.
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
                run_function_calls_tests = .false.
            else
                print *, '  OK: "sqrt(abs(x))" parsed'
            end if
        end block
        
        if (run_function_calls_tests) then
            print *, 'PASS: Function calls'
        end if
        
    end function run_function_calls_tests

end program test_function_calls