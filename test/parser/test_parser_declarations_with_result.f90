program test_parser_declarations_with_result
    use lexer_core, only: tokenize_safe, tokenize_result_t
    use parser_state_module, only: create_parser_state
    use ast_arena, only: ast_arena_t, create_ast_arena
    use parser_declarations, only: parse_declaration_with_result
    use parser_result_types, only: parse_result_t
    implicit none

    print *, "=== Parser Declarations with Result Test ==="

    ! Test successful parsing
    call test_successful_declaration()
    call test_integer_array_declaration()
    call test_character_declaration()

    ! Test error cases with structured error handling
    call test_missing_double_colon()
    call test_missing_variable_name()
    call test_missing_closing_paren()

    print *, ""
    print *, "=== ALL TESTS PASSED ==="
    print *, "Parser declarations with result_t working correctly!"

contains

    subroutine test_successful_declaration()
        type(ast_arena_t) :: arena
        type(parse_result_t) :: parse_res
        character(len=*), parameter :: source = "integer :: x"
        
        arena = create_ast_arena()
        parse_res = parse_declaration_tokens(arena, source)
        
        if (.not. parse_res%is_success()) then
            print *, "FAIL: Simple declaration should succeed"
            print *, "Error: ", parse_res%result%get_message()
            stop 1
        end if
        
        if (parse_res%get_node() <= 0) then
            print *, "FAIL: Successful parse should return valid node index"
            stop 1
        end if
        
        print *, "PASS: Simple declaration parsing"
    end subroutine test_successful_declaration

    subroutine test_integer_array_declaration()
        type(ast_arena_t) :: arena
        type(parse_result_t) :: parse_res
        character(len=*), parameter :: source = "integer :: arr(10)"
        
        arena = create_ast_arena()
        parse_res = parse_declaration_tokens(arena, source)
        
        if (.not. parse_res%is_success()) then
            print *, "FAIL: Array declaration should succeed"
            print *, "Error: ", parse_res%result%get_message()
            stop 1
        end if
        
        print *, "PASS: Array declaration parsing"
    end subroutine test_integer_array_declaration

    subroutine test_character_declaration()
        type(ast_arena_t) :: arena
        type(parse_result_t) :: parse_res
        character(len=*), parameter :: source = "character(len=80) :: name"
        
        arena = create_ast_arena()
        parse_res = parse_declaration_tokens(arena, source)
        
        if (.not. parse_res%is_success()) then
            print *, "FAIL: Character declaration should succeed"
            print *, "Error: ", parse_res%result%get_message()
            stop 1
        end if
        
        print *, "PASS: Character declaration parsing"
    end subroutine test_character_declaration

    subroutine test_missing_double_colon()
        type(ast_arena_t) :: arena
        type(parse_result_t) :: parse_res
        character(len=*), parameter :: source = "integer x"  ! Missing ::
        
        arena = create_ast_arena()
        parse_res = parse_declaration_tokens(arena, source)
        
        if (parse_res%is_success()) then
            print *, "FAIL: Missing :: should cause error"
            stop 1
        end if
        
        ! Verify we get a structured error
        if (len_trim(parse_res%result%get_message()) == 0) then
            print *, "FAIL: Error should have message"
            stop 1
        end if
        
        print *, "PASS: Missing :: error handling"
    end subroutine test_missing_double_colon

    subroutine test_missing_variable_name()
        type(ast_arena_t) :: arena
        type(parse_result_t) :: parse_res
        character(len=*), parameter :: source = "integer ::"  ! Missing variable name
        
        arena = create_ast_arena()
        parse_res = parse_declaration_tokens(arena, source)
        
        if (parse_res%is_success()) then
            print *, "FAIL: Missing variable name should cause error"
            stop 1
        end if
        
        print *, "PASS: Missing variable name error handling"
    end subroutine test_missing_variable_name

    subroutine test_missing_closing_paren()
        type(ast_arena_t) :: arena
        type(parse_result_t) :: parse_res
        character(len=*), parameter :: source = "integer :: arr(10"  ! Missing )
        
        arena = create_ast_arena()
        parse_res = parse_declaration_tokens(arena, source)
        
        if (parse_res%is_success()) then
            print *, "FAIL: Missing closing paren should cause error"
            stop 1
        end if
        
        print *, "PASS: Missing closing paren error handling"
    end subroutine test_missing_closing_paren

    ! Helper function to parse declaration from source string
    function parse_declaration_tokens(arena, source) result(parse_res)
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: source
        type(parse_result_t) :: parse_res
        
        block
            use lexer_core, only: token_t, tokenize_result_t, tokenize_safe
            use parser_state_module, only: parser_state_t, create_parser_state
            
            type(tokenize_result_t) :: lex_result
            type(parser_state_t) :: parser
            
            lex_result = tokenize_safe(source)
            if (lex_result%result%is_failure()) then
                call parse_res%set_error("Tokenization failed", 500, "test", "parse_declaration_tokens")
                return
            end if
            
            parser = create_parser_state(lex_result%tokens)
            parse_res = parse_declaration_with_result(parser, arena)
        end block
    end function parse_declaration_tokens

end program test_parser_declarations_with_result