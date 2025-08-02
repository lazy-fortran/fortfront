program test_character_substring
    use ast_core
    use ast_nodes_core, only: character_substring_node
    use ast_factory
    use parser_expressions_module, only: parse_expression
    use lexer_core, only: token_t
    use frontend, only: lex_source
    use codegen_core
    implicit none
    
    logical :: all_passed
    all_passed = .true.
    
    call test_simple_substring(all_passed)
    call test_substring_no_start(all_passed)
    call test_substring_no_end(all_passed)
    call test_substring_variables(all_passed)
    call test_substring_complex_expr(all_passed)
    call test_nested_substring(all_passed)
    call test_substring_after_component_access(all_passed)
    
    if (all_passed) then
        print *, "All character substring tests passed!"
    else
        print *, "Some character substring tests FAILED!"
        stop 1
    end if
    
contains

    subroutine test_simple_substring(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        character(len=100) :: test_name
        character(len=:), allocatable :: source, code, error_msg
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        logical :: test_passed
        
        test_name = "simple substring str(1:5)"
        source = "str(1:5)"
        
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        expr_index = parse_expression(tokens, arena)
        
        ! Check that we created a character_substring_node
        test_passed = .false.
        if (expr_index > 0 .and. expr_index <= arena%size) then
            select type (node => arena%entries(expr_index)%node)
            type is (character_substring_node)
                test_passed = .true.
                ! Generate code to verify
                code = generate_code_from_arena(arena, expr_index)
                if (code /= "str(1:5)") then
                    print *, "FAIL: ", trim(test_name), " - incorrect code generation"
                    print *, "  Expected: str(1:5)"
                    print *, "  Got: ", code
                    test_passed = .false.
                end if
            class default
                print *, "FAIL: ", trim(test_name), " - wrong node type"
                print *, "  Got node type: ", arena%entries(expr_index)%node_type
                ! Also print generated code to understand what was created
                code = generate_code_from_arena(arena, expr_index)
                print *, "  Generated code: ", code
            end select
        else
            print *, "FAIL: ", trim(test_name), " - invalid expression index"
        end if
        
        if (test_passed) then
            print *, "PASS: ", trim(test_name)
        else
            all_passed = .false.
        end if
        
        ! No cleanup needed for arena
    end subroutine test_simple_substring
    
    subroutine test_substring_no_start(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        character(len=100) :: test_name
        character(len=:), allocatable :: source, code, error_msg
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        logical :: test_passed
        
        test_name = "substring with no start str(:5)"
        source = "str(:5)"
        
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        expr_index = parse_expression(tokens, arena)
        
        test_passed = .false.
        if (expr_index > 0 .and. expr_index <= arena%size) then
            select type (node => arena%entries(expr_index)%node)
            type is (character_substring_node)
                test_passed = .true.
                code = generate_code_from_arena(arena, expr_index)
                if (code /= "str(:5)") then
                    print *, "FAIL: ", trim(test_name), " - incorrect code generation"
                    print *, "  Expected: str(:5)"
                    print *, "  Got: ", code
                    test_passed = .false.
                end if
            class default
                print *, "FAIL: ", trim(test_name), " - wrong node type"
            end select
        else
            print *, "FAIL: ", trim(test_name), " - invalid expression index"
        end if
        
        if (test_passed) then
            print *, "PASS: ", trim(test_name)
        else
            all_passed = .false.
        end if
        
        ! No cleanup needed for arena
    end subroutine test_substring_no_start
    
    subroutine test_substring_no_end(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        character(len=100) :: test_name
        character(len=:), allocatable :: source, code, error_msg
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        logical :: test_passed
        
        test_name = "substring with no end str(3:)"
        source = "str(3:)"
        
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        expr_index = parse_expression(tokens, arena)
        
        test_passed = .false.
        if (expr_index > 0 .and. expr_index <= arena%size) then
            select type (node => arena%entries(expr_index)%node)
            type is (character_substring_node)
                test_passed = .true.
                code = generate_code_from_arena(arena, expr_index)
                if (code /= "str(3:)") then
                    print *, "FAIL: ", trim(test_name), " - incorrect code generation"
                    print *, "  Expected: str(3:)"
                    print *, "  Got: ", code
                    test_passed = .false.
                end if
            class default
                print *, "FAIL: ", trim(test_name), " - wrong node type"
            end select
        else
            print *, "FAIL: ", trim(test_name), " - invalid expression index"
        end if
        
        if (test_passed) then
            print *, "PASS: ", trim(test_name)
        else
            all_passed = .false.
        end if
        
        ! No cleanup needed for arena
    end subroutine test_substring_no_end
    
    subroutine test_substring_variables(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        character(len=100) :: test_name
        character(len=:), allocatable :: source, code, error_msg
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        logical :: test_passed
        
        test_name = "substring with variable indices str(i:j)"
        source = "str(i:j)"
        
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        expr_index = parse_expression(tokens, arena)
        
        test_passed = .false.
        if (expr_index > 0 .and. expr_index <= arena%size) then
            select type (node => arena%entries(expr_index)%node)
            type is (character_substring_node)
                test_passed = .true.
                code = generate_code_from_arena(arena, expr_index)
                if (code /= "str(i:j)") then
                    print *, "FAIL: ", trim(test_name), " - incorrect code generation"
                    print *, "  Expected: str(i:j)"
                    print *, "  Got: ", code
                    test_passed = .false.
                end if
            class default
                print *, "FAIL: ", trim(test_name), " - wrong node type"
            end select
        else
            print *, "FAIL: ", trim(test_name), " - invalid expression index"
        end if
        
        if (test_passed) then
            print *, "PASS: ", trim(test_name)
        else
            all_passed = .false.
        end if
        
        ! No cleanup needed for arena
    end subroutine test_substring_variables
    
    subroutine test_substring_complex_expr(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        character(len=100) :: test_name
        character(len=:), allocatable :: source, code, error_msg
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        logical :: test_passed
        
        test_name = "substring with expressions str(i+1:j*2)"
        source = "str(i+1:j*2)"
        
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        expr_index = parse_expression(tokens, arena)
        
        test_passed = .false.
        if (expr_index > 0 .and. expr_index <= arena%size) then
            select type (node => arena%entries(expr_index)%node)
            type is (character_substring_node)
                test_passed = .true.
                code = generate_code_from_arena(arena, expr_index)
                if (code /= "str(i + 1:j*2)") then
                    print *, "FAIL: ", trim(test_name), " - incorrect code generation"
                    print *, "  Expected: str(i + 1:j*2)"
                    print *, "  Got: ", code
                    test_passed = .false.
                end if
            class default
                print *, "FAIL: ", trim(test_name), " - wrong node type"
            end select
        else
            print *, "FAIL: ", trim(test_name), " - invalid expression index"
        end if
        
        if (test_passed) then
            print *, "PASS: ", trim(test_name)
        else
            all_passed = .false.
        end if
        
        ! No cleanup needed for arena
    end subroutine test_substring_complex_expr
    
    subroutine test_nested_substring(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        character(len=100) :: test_name
        character(len=:), allocatable :: source, code, error_msg
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        logical :: test_passed
        
        test_name = "nested substring operations"
        source = "str(2:8)(1:3)"
        
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        expr_index = parse_expression(tokens, arena)
        
        test_passed = .false.
        if (expr_index > 0 .and. expr_index <= arena%size) then
            ! The outer operation should be a substring
            select type (node => arena%entries(expr_index)%node)
            type is (character_substring_node)
                test_passed = .true.
                code = generate_code_from_arena(arena, expr_index)
                if (code /= "str(2:8)(1:3)") then
                    print *, "FAIL: ", trim(test_name), " - incorrect code generation"
                    print *, "  Expected: str(2:8)(1:3)"
                    print *, "  Got: ", code
                    test_passed = .false.
                end if
            class default
                print *, "FAIL: ", trim(test_name), " - wrong node type"
            end select
        else
            print *, "FAIL: ", trim(test_name), " - invalid expression index"
        end if
        
        if (test_passed) then
            print *, "PASS: ", trim(test_name)
        else
            all_passed = .false.
        end if
        
        ! No cleanup needed for arena
    end subroutine test_nested_substring
    
    subroutine test_substring_after_component_access(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        character(len=100) :: test_name
        character(len=:), allocatable :: source, code, error_msg
        type(token_t), allocatable :: tokens(:)
        integer :: expr_index
        logical :: test_passed
        
        test_name = "substring after component access"
        source = "obj%name(1:10)"
        
        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        expr_index = parse_expression(tokens, arena)
        
        test_passed = .false.
        if (expr_index > 0 .and. expr_index <= arena%size) then
            ! Should create a character substring on the component access
            select type (node => arena%entries(expr_index)%node)
            type is (character_substring_node)
                test_passed = .true.
                code = generate_code_from_arena(arena, expr_index)
                if (code /= "obj%name(1:10)") then
                    print *, "FAIL: ", trim(test_name), " - incorrect code generation"
                    print *, "  Expected: obj%name(1:10)"
                    print *, "  Got: ", code
                    test_passed = .false.
                end if
            class default
                print *, "FAIL: ", trim(test_name), " - wrong node type"
            end select
        else
            print *, "FAIL: ", trim(test_name), " - invalid expression index"
        end if
        
        if (test_passed) then
            print *, "PASS: ", trim(test_name)
        else
            all_passed = .false.
        end if
        
        ! No cleanup needed for arena
    end subroutine test_substring_after_component_access

end program test_character_substring