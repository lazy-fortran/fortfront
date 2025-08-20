! Parser test utilities module
! Common helper functions for parser expression tests
module parser_test_utilities
    use ast_core, only: ast_arena_t, create_ast_arena, ast_node, binary_op_node, &
                        array_literal_node, call_or_subscript_node, literal_node, &
                        identifier_node, LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, &
                        LITERAL_LOGICAL
    use ast_nodes_core, only: component_access_node
    use parser_expressions_module
    use lexer_core
    implicit none

    private
    public :: test_binary_expr, verify_precedence_structure, &
              verify_precedence_structure_left, parse_and_verify, &
              test_result_t, create_test_result

    type :: test_result_t
        logical :: passed
        character(len=256) :: message
    end type test_result_t

contains

    ! Create a test result
    function create_test_result(passed, message) result(result)
        logical, intent(in) :: passed
        character(len=*), intent(in) :: message
        type(test_result_t) :: result
        
        result%passed = passed
        result%message = message
    end function create_test_result

    ! Parse expression and verify it's a binary operator with expected properties
    logical function test_binary_expr(expr_str, expected_op, expected_left, &
                                      expected_right, arena)
        character(len=*), intent(in) :: expr_str, expected_op, expected_left, &
                                        expected_right
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        test_binary_expr = .true.
        
        arena = create_ast_arena()
        call tokenize_core(expr_str, tokens)
        expr_index = parse_expression(tokens, arena)
        
        if (expr_index == 0) then
            print *, '  FAIL: Failed to parse "', trim(expr_str), '"'
            test_binary_expr = .false.
            return
        end if
        
        ! Verify it's a binary op with expected operator
        select type (node => arena%entries(expr_index)%node)
        class is (binary_op_node)
            if (node%operator /= expected_op) then
                print *, '  FAIL: Expected operator "', expected_op, &
                         '" but got "', node%operator, '"'
                test_binary_expr = .false.
            else
                print *, '  OK: "', trim(expr_str), '" parsed correctly'
            end if
        class default
            print *, '  FAIL: Expected binary operator node for "', &
                     trim(expr_str), '"'
            test_binary_expr = .false.
        end select
        
    end function test_binary_expr

    ! Helper to verify precedence structure
    logical function verify_precedence_structure(expr_index, root_op, child_op, &
                                                  arena)
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: root_op, child_op
        type(ast_arena_t), intent(in) :: arena
        
        verify_precedence_structure = .false.
        
        select type (root => arena%entries(expr_index)%node)
        class is (binary_op_node)
            if (root%operator == root_op) then
                ! Check right child
                select type (right => arena%entries(root%right_index)%node)
                class is (binary_op_node)
                    if (right%operator == child_op) then
                        verify_precedence_structure = .true.
                    end if
                end select
            end if
        end select
        
    end function verify_precedence_structure

    ! Helper to verify precedence structure with left child
    logical function verify_precedence_structure_left(expr_index, root_op, &
                                                       child_op, arena)
        integer, intent(in) :: expr_index
        character(len=*), intent(in) :: root_op, child_op
        type(ast_arena_t), intent(in) :: arena
        
        verify_precedence_structure_left = .false.
        
        select type (root => arena%entries(expr_index)%node)
        class is (binary_op_node)
            if (root%operator == root_op) then
                ! Check left child
                select type (left => arena%entries(root%left_index)%node)
                class is (binary_op_node)
                    if (left%operator == child_op) then
                        verify_precedence_structure_left = .true.
                    end if
                end select
            end if
        end select
        
    end function verify_precedence_structure_left

    ! General parse and verify function
    logical function parse_and_verify(expr_str, arena)
        character(len=*), intent(in) :: expr_str
        type(ast_arena_t), intent(inout) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        
        arena = create_ast_arena()
        call tokenize_core(expr_str, tokens)
        expr_index = parse_expression(tokens, arena)
        
        parse_and_verify = (expr_index /= 0)
        
        if (.not. parse_and_verify) then
            print *, '  FAIL: Failed to parse "', trim(expr_str), '"'
        else
            print *, '  OK: "', trim(expr_str), '" parsed successfully'
        end if
        
    end function parse_and_verify


end module parser_test_utilities