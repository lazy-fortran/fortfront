program test_member_access
    use ast_core, only: ast_arena_t, create_ast_arena, binary_op_node
    use ast_nodes_core, only: component_access_node
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.
    print *, '=== Member Access Tests ==='
    print *

    arena = create_ast_arena()

    if (.not. run_member_access_tests()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All member access tests passed!'
        stop 0
    else
        print *, 'Some member access tests failed!'
        stop 1
    end if

contains

    logical function run_member_access_tests()
        integer :: expr_index
        
        run_member_access_tests = .true.
        print *, 'Test 1: Member access operator (%)'
        
        ! Test simple member access
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("obj%field", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "obj%field"'
                run_member_access_tests = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                type is (component_access_node)
                    if (node%component_name == "field") then
                        print *, '  OK: "obj%field" parsed as component access'
                    else
                        print *, '  FAIL: Expected component name "field", got: ', node%component_name
                        run_member_access_tests = .false.
                    end if
                type is (binary_op_node)
                    ! Backward compatibility - some parsers might still use binary op
                    if (node%operator == "%") then
                        print *, '  OK: "obj%field" parsed as member access (legacy)'
                    else
                        print *, '  FAIL: Expected % operator'
                        run_member_access_tests = .false.
                    end if
                class default
                    print *, '  FAIL: Expected component_access_node for member access'
                    run_member_access_tests = .false.
                end select
            end if
        end block
        
        ! Test nested member access
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("obj%nested%field", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse nested member access'
                run_member_access_tests = .false.
            else
                print *, '  OK: "obj%nested%field" parsed'
            end if
        end block
        
        if (run_member_access_tests) then
            print *, 'PASS: Member access operator'
        end if
        
    end function run_member_access_tests

end program test_member_access