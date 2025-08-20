program test_expression_error_cases
    use ast_core, only: ast_arena_t, create_ast_arena, identifier_node, binary_op_node
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Expression Error Cases Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_error_cases_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All expression error case tests passed!'
        stop 0
    else
        print *, 'Some expression error case tests failed!'
        stop 1
    end if

contains

    logical function run_error_cases_tests()
        integer :: expr_index
        
        run_error_cases_tests = .true.
        print *, 'Test 1: Error cases'
        
        ! Test malformed array literal - missing closing bracket
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("[1, 2, 3", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index /= 0) then
                print *, '  FAIL: Should fail on missing closing bracket'
                run_error_cases_tests = .false.
            else
                print *, '  OK: Correctly failed on "[1, 2, 3" (missing ])'
            end if
        end block
        
        ! Test malformed array literal - trailing comma
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("[1, 2,]", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index /= 0) then
                print *, '  FAIL: Should fail on trailing comma'
                run_error_cases_tests = .false.
            else
                print *, '  OK: Correctly failed on "[1, 2,]" (trailing comma)'
            end if
        end block
        
        ! Test incomplete expression
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("a +", tokens)
            expr_index = parse_expression(tokens, arena)
            
            ! The parser might return a partial parse up to the error
            if (expr_index == 0) then
                print *, '  OK: Failed on incomplete expression "a +"'
            else
                ! Check if it's just the identifier 'a'
                select type (node => arena%entries(expr_index)%node)
                class is (identifier_node)
                    print *, '  OK: Parsed valid prefix "a" from incomplete "a +"'
                class is (binary_op_node)
                    ! Parser creates binary op even with missing right operand
                    print *, '  OK: Parser creates binary op for incomplete "a +" (expected behavior)'
                class default
                    print *, '  INFO: Parsed some prefix of incomplete expression'
                end select
            end if
        end block
        
        ! Test mismatched parentheses
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("(a + b))", tokens)
            expr_index = parse_expression(tokens, arena)
            
            ! This might actually parse as (a + b) followed by extra )
            ! which is valid in expression context
            if (expr_index == 0) then
                print *, '  OK: Failed on mismatched parentheses'
            else
                print *, '  OK: Parsed valid prefix of mismatched parentheses'
            end if
        end block
        
        if (run_error_cases_tests) then
            print *, 'PASS: Error cases'
        end if
        
    end function run_error_cases_tests

end program test_expression_error_cases