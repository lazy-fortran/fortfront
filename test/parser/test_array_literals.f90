program test_array_literals
    use ast_core, only: ast_arena_t, create_ast_arena, array_literal_node
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Array Literals Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_array_literals_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All array literal tests passed!'
        stop 0
    else
        print *, 'Some array literal tests failed!'
        stop 1
    end if

contains

    logical function run_array_literals_tests()
        integer :: expr_index
        
        run_array_literals_tests = .true.
        print *, 'Test 1: Array literals'
        
        ! Test simple integer array
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("[1, 2, 3]", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "[1, 2, 3]"'
                run_array_literals_tests = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                class is (array_literal_node)
                    if (size(node%element_indices) /= 3) then
                        print *, '  FAIL: Expected 3 elements in array'
                        run_array_literals_tests = .false.
                    else
                        print *, '  OK: "[1, 2, 3]" parsed as array with 3 elements'
                    end if
                class default
                    print *, '  FAIL: Expected array literal node'
                    run_array_literals_tests = .false.
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
                run_array_literals_tests = .false.
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
                run_array_literals_tests = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                class is (array_literal_node)
                    if (size(node%element_indices) /= 0) then
                        print *, '  FAIL: Expected 0 elements in empty array'
                        run_array_literals_tests = .false.
                    else
                        print *, '  OK: "[]" parsed as empty array'
                    end if
                class default
                    print *, '  FAIL: Expected array literal node for empty array'
                    run_array_literals_tests = .false.
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
                run_array_literals_tests = .false.
            else
                print *, '  OK: "[42]" parsed'
            end if
        end block
        
        if (run_array_literals_tests) then
            print *, 'PASS: Array literals'
        end if
        
    end function run_array_literals_tests

end program test_array_literals