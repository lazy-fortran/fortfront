program test_logical_literals
    use ast_core, only: ast_arena_t, create_ast_arena, literal_node
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Logical Literals Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_logical_literals_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All logical literal tests passed!'
        stop 0
    else
        print *, 'Some logical literal tests failed!'
        stop 1
    end if

contains

    logical function run_logical_literals_tests()
        integer :: expr_index
        
        run_logical_literals_tests = .true.
        print *, 'Test 1: Logical literals'
        
        ! Test .true.
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core(".true.", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse ".true."'
                run_logical_literals_tests = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                class is (literal_node)
                    if (node%value == ".true.") then
                        print *, '  OK: ".true." parsed as logical literal'
                    else
                        print *, '  FAIL: Incorrect value for .true.'
                        run_logical_literals_tests = .false.
                    end if
                class default
                    print *, '  FAIL: Expected literal node for .true.'
                    run_logical_literals_tests = .false.
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
                run_logical_literals_tests = .false.
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
                run_logical_literals_tests = .false.
            else
                print *, '  OK: Logical literal in expression parsed'
            end if
        end block
        
        if (run_logical_literals_tests) then
            print *, 'PASS: Logical literals'
        end if
        
    end function run_logical_literals_tests

end program test_logical_literals