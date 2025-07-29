program test_frontend_parser_explicit_types
    use lexer_core
    use ast_core
    use ast_factory
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_statements_module, only: parse_function_definition, parse_subroutine_definition
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Testing Parser: Explicit Type Declarations ==='
    print *

    ! Test basic explicit type declaration
    if (.not. test_basic_explicit_type()) all_passed = .false.

    ! Test with intent
    if (.not. test_explicit_type_with_intent()) all_passed = .false.

    ! Test multiple parameters with same type
    if (.not. test_multiple_params_same_type()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All explicit type parser tests passed!'
        stop 0
    else
        print *, 'Some explicit type parser tests failed!'
        stop 1
    end if

contains

    logical function test_basic_explicit_type()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: func_index

        test_basic_explicit_type = .false.
        print *, 'Testing basic explicit type declaration...'

        ! Create tokens for: function add(real(8) :: x, y) result(z)
        allocate (tokens(20))
        tokens(1) = token_t(TK_KEYWORD, "function", 1, 1)
        tokens(2) = token_t(TK_IDENTIFIER, "add", 1, 10)
        tokens(3) = token_t(TK_OPERATOR, "(", 1, 13)
        tokens(4) = token_t(TK_KEYWORD, "real", 1, 14)
        tokens(5) = token_t(TK_OPERATOR, "(", 1, 18)
        tokens(6) = token_t(TK_NUMBER, "8", 1, 19)
        tokens(7) = token_t(TK_OPERATOR, ")", 1, 20)
        tokens(8) = token_t(TK_OPERATOR, "::", 1, 22)
        tokens(9) = token_t(TK_IDENTIFIER, "x", 1, 25)
        tokens(10) = token_t(TK_OPERATOR, ",", 1, 26)
        tokens(11) = token_t(TK_IDENTIFIER, "y", 1, 28)
        tokens(12) = token_t(TK_OPERATOR, ")", 1, 29)
        tokens(13) = token_t(TK_KEYWORD, "result", 1, 31)
        tokens(14) = token_t(TK_OPERATOR, "(", 1, 38)
        tokens(15) = token_t(TK_IDENTIFIER, "z", 1, 39)
        tokens(16) = token_t(TK_OPERATOR, ")", 1, 40)
        tokens(17) = token_t(TK_NEWLINE, char(10), 1, 41)
        tokens(18) = token_t(TK_KEYWORD, "end", 2, 1)
        tokens(19) = token_t(TK_KEYWORD, "function", 2, 5)
        tokens(20) = token_t(TK_EOF, "", 2, 13)

        ! Parse
        parser = create_parser_state(tokens)
        func_index = parse_function_definition(parser, arena)

        if (func_index > 0) then
            print *, '  PASS: Parsed function with explicit type parameters'
            test_basic_explicit_type = .true.
        else
            print *, '  FAIL: Could not parse function with explicit types'
        end if

    end function test_basic_explicit_type

    logical function test_explicit_type_with_intent()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: sub_index

        test_explicit_type_with_intent = .false.
        print *, 'Testing explicit type with intent...'

        ! Create tokens for: subroutine process(real(8), intent(in) :: a, b)
        allocate (tokens(25))
        tokens(1) = token_t(TK_KEYWORD, "subroutine", 1, 1)
        tokens(2) = token_t(TK_IDENTIFIER, "process", 1, 12)
        tokens(3) = token_t(TK_OPERATOR, "(", 1, 19)
        tokens(4) = token_t(TK_KEYWORD, "real", 1, 20)
        tokens(5) = token_t(TK_OPERATOR, "(", 1, 24)
        tokens(6) = token_t(TK_NUMBER, "8", 1, 25)
        tokens(7) = token_t(TK_OPERATOR, ")", 1, 26)
        tokens(8) = token_t(TK_OPERATOR, ",", 1, 27)
        tokens(9) = token_t(TK_KEYWORD, "intent", 1, 29)
        tokens(10) = token_t(TK_OPERATOR, "(", 1, 35)
        tokens(11) = token_t(TK_KEYWORD, "in", 1, 36)
        tokens(12) = token_t(TK_OPERATOR, ")", 1, 38)
        tokens(13) = token_t(TK_OPERATOR, "::", 1, 40)
        tokens(14) = token_t(TK_IDENTIFIER, "a", 1, 43)
        tokens(15) = token_t(TK_OPERATOR, ",", 1, 44)
        tokens(16) = token_t(TK_IDENTIFIER, "b", 1, 46)
        tokens(17) = token_t(TK_OPERATOR, ")", 1, 47)
        tokens(18) = token_t(TK_NEWLINE, char(10), 1, 48)
        tokens(19) = token_t(TK_KEYWORD, "print", 2, 5)
        tokens(20) = token_t(TK_OPERATOR, "*", 2, 11)
        tokens(21) = token_t(TK_NEWLINE, char(10), 2, 12)
        tokens(22) = token_t(TK_KEYWORD, "end", 3, 1)
        tokens(23) = token_t(TK_KEYWORD, "subroutine", 3, 5)
        tokens(24) = token_t(TK_NEWLINE, char(10), 3, 15)
        tokens(25) = token_t(TK_EOF, "", 4, 1)

        ! Parse
        parser = create_parser_state(tokens)
        sub_index = parse_subroutine_definition(parser, arena)

        if (sub_index > 0) then
            print *, '  PASS: Parsed subroutine with intent(in) parameters'
            test_explicit_type_with_intent = .true.
        else
            print *, '  FAIL: Could not parse subroutine with intent'
        end if

    end function test_explicit_type_with_intent

    logical function test_multiple_params_same_type()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: func_index

        test_multiple_params_same_type = .false.
        print *, 'Testing multiple parameters with same type...'

        ! Create tokens for: function sum3(integer :: a, b, c)
        allocate (tokens(15))
        tokens(1) = token_t(TK_KEYWORD, "function", 1, 1)
        tokens(2) = token_t(TK_IDENTIFIER, "sum3", 1, 10)
        tokens(3) = token_t(TK_OPERATOR, "(", 1, 14)
        tokens(4) = token_t(TK_KEYWORD, "integer", 1, 15)
        tokens(5) = token_t(TK_OPERATOR, "::", 1, 23)
        tokens(6) = token_t(TK_IDENTIFIER, "a", 1, 26)
        tokens(7) = token_t(TK_OPERATOR, ",", 1, 27)
        tokens(8) = token_t(TK_IDENTIFIER, "b", 1, 29)
        tokens(9) = token_t(TK_OPERATOR, ",", 1, 30)
        tokens(10) = token_t(TK_IDENTIFIER, "c", 1, 32)
        tokens(11) = token_t(TK_OPERATOR, ")", 1, 33)
        tokens(12) = token_t(TK_NEWLINE, char(10), 1, 34)
        tokens(13) = token_t(TK_KEYWORD, "end", 2, 1)
        tokens(14) = token_t(TK_KEYWORD, "function", 2, 5)
        tokens(15) = token_t(TK_EOF, "", 2, 13)

        ! Parse
        parser = create_parser_state(tokens)
        func_index = parse_function_definition(parser, arena)

        if (func_index > 0) then
            print *, '  PASS: Parsed multiple parameters with same type'
            test_multiple_params_same_type = .true.
        else
            print *, '  FAIL: Could not parse multiple params of same type'
        end if

    end function test_multiple_params_same_type

end program test_frontend_parser_explicit_types
