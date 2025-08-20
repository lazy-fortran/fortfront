program test_parser_arrays_functions
    use ast_core, only: ast_arena_t, create_ast_arena, array_literal_node
    use ast_nodes_core, only: component_access_node
    use ast_core, only: binary_op_node, literal_node
    use parser_test_utilities
    use parser_expressions_module
    use lexer_core
    implicit none

    logical :: all_passed
    type(ast_arena_t) :: arena

    all_passed = .true.

    print *, '=== Parser Arrays and Functions Tests ==='
    print *

    ! Initialize arena
    arena = create_ast_arena()

    ! Run array and function tests
    if (.not. test_array_literals()) all_passed = .false.
    if (.not. test_function_calls()) all_passed = .false.
    if (.not. test_member_access()) all_passed = .false.
    if (.not. test_logical_literals()) all_passed = .false.
    if (.not. test_large_arrays()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All parser arrays and functions tests passed!'
        stop 0
    else
        print *, 'Some parser arrays and functions tests failed!'
        stop 1
    end if

contains

    logical function test_array_literals()
        integer :: expr_index
        
        test_array_literals = .true.
        print *, 'Test: Array literals'
        
        ! Test simple integer array
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("[1, 2, 3]", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "[1, 2, 3]"'
                test_array_literals = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                class is (array_literal_node)
                    if (size(node%element_indices) /= 3) then
                        print *, '  FAIL: Expected 3 elements in array'
                        test_array_literals = .false.
                    else
                        print *, '  OK: "[1, 2, 3]" parsed as array with 3 elements'
                    end if
                class default
                    print *, '  FAIL: Expected array literal node'
                    test_array_literals = .false.
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
                test_array_literals = .false.
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
                test_array_literals = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                class is (array_literal_node)
                    if (size(node%element_indices) /= 0) then
                        print *, '  FAIL: Expected 0 elements in empty array'
                        test_array_literals = .false.
                    else
                        print *, '  OK: "[]" parsed as empty array'
                    end if
                class default
                    print *, '  FAIL: Expected array literal node for empty array'
                    test_array_literals = .false.
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
                test_array_literals = .false.
            else
                print *, '  OK: "[42]" parsed'
            end if
        end block
        
        ! Test mixed element array with expressions
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("[a, b+c, 42]", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse mixed element array'
                test_array_literals = .false.
            else
                print *, '  OK: "[a, b+c, 42]" parsed'
            end if
        end block
        
        if (test_array_literals) then
            print *, 'PASS: Array literals'
        end if
        
    end function test_array_literals

    logical function test_function_calls()
        integer :: expr_index
        
        test_function_calls = .true.
        print *, 'Test: Function calls'
        
        ! Test no-arg function
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("random()", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "random()"'
                test_function_calls = .false.
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
                test_function_calls = .false.
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
                test_function_calls = .false.
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
                test_function_calls = .false.
            else
                print *, '  OK: "sqrt(abs(x))" parsed'
            end if
        end block
        
        ! Test function call with array argument
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("sum([1, 2, 3])", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "sum([1, 2, 3])"'
                test_function_calls = .false.
            else
                print *, '  OK: "sum([1, 2, 3])" parsed'
            end if
        end block
        
        if (test_function_calls) then
            print *, 'PASS: Function calls'
        end if
        
    end function test_function_calls

    logical function test_member_access()
        integer :: expr_index
        
        test_member_access = .true.
        print *, 'Test: Member access operator (%)'
        
        ! Test simple member access
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("obj%field", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "obj%field"'
                test_member_access = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                type is (component_access_node)
                    if (node%component_name == "field") then
                        print *, '  OK: "obj%field" parsed as component access'
                    else
                        print *, '  FAIL: Expected component name "field", got: ', &
                                 node%component_name
                        test_member_access = .false.
                    end if
                type is (binary_op_node)
                    ! Backward compatibility - some parsers might still use binary op
                    if (node%operator == "%") then
                        print *, '  OK: "obj%field" parsed as member access (legacy)'
                    else
                        print *, '  FAIL: Expected % operator'
                        test_member_access = .false.
                    end if
                class default
                    print *, '  FAIL: Expected component_access_node for member access'
                    test_member_access = .false.
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
                test_member_access = .false.
            else
                print *, '  OK: "obj%nested%field" parsed'
            end if
        end block
        
        ! Test member access with function call
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core("obj%method()", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse "obj%method()"'
                test_member_access = .false.
            else
                print *, '  OK: "obj%method()" parsed'
            end if
        end block
        
        if (test_member_access) then
            print *, 'PASS: Member access operator'
        end if
        
    end function test_member_access

    logical function test_logical_literals()
        integer :: expr_index
        
        test_logical_literals = .true.
        print *, 'Test: Logical literals'
        
        ! Test .true.
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core(".true.", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse ".true."'
                test_logical_literals = .false.
            else
                print *, '  OK: ".true." parsed'
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
                test_logical_literals = .false.
            else
                print *, '  OK: ".false." parsed'
            end if
        end block
        
        ! Test logical literal in expression
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core(".true. .and. .false.", tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse ".true. .and. .false."'
                test_logical_literals = .false.
            else
                print *, '  OK: ".true. .and. .false." parsed'
            end if
        end block
        
        if (test_logical_literals) then
            print *, 'PASS: Logical literals'
        end if
        
    end function test_logical_literals

    logical function test_large_arrays()
        integer :: expr_index
        character(len=1024) :: large_array_str
        integer :: i
        
        test_large_arrays = .true.
        print *, 'Test: Large arrays (>100 elements)'
        
        ! Build a string with 105 elements
        large_array_str = "["
        do i = 1, 105
            if (i > 1) large_array_str = trim(large_array_str) // ", "
            write(large_array_str, '(A,I0)') trim(large_array_str), i
        end do
        large_array_str = trim(large_array_str) // "]"
        
        arena = create_ast_arena()
        block
            type(token_t), allocatable :: tokens(:)
            call tokenize_core(trim(large_array_str), tokens)
            expr_index = parse_expression(tokens, arena)
            
            if (expr_index == 0) then
                print *, '  FAIL: Failed to parse large array'
                test_large_arrays = .false.
            else
                select type (node => arena%entries(expr_index)%node)
                class is (array_literal_node)
                    if (size(node%element_indices) == 105) then
                        print *, '  OK: Large array with 105 elements parsed'
                    else
                        print *, '  FAIL: Expected 105 elements, got', &
                                 size(node%element_indices)
                        test_large_arrays = .false.
                    end if
                class default
                    print *, '  FAIL: Expected array literal node'
                    test_large_arrays = .false.
                end select
            end if
        end block
        
        if (test_large_arrays) then
            print *, 'PASS: Large arrays'
        end if
        
    end function test_large_arrays

end program test_parser_arrays_functions