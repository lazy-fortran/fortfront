program test_implicit_statement_parsing
    use ast_core
    use ast_factory
    use parser_statements_module, only: parse_implicit_statement
    use parser_state_module
    use lexer_core
    use codegen_core, only: generate_code_from_arena
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Implicit Statement Parsing Tests ==="
    
    all_tests_passed = .true.
    
    ! Test implicit none
    if (.not. test_implicit_none()) all_tests_passed = .false.
    
    ! Test implicit type with letter ranges
    if (.not. test_implicit_real_range()) all_tests_passed = .false.
    if (.not. test_implicit_integer_range()) all_tests_passed = .false.
    
    ! Test implicit type with kind specification
    if (.not. test_implicit_real_with_kind()) all_tests_passed = .false.
    
    ! Test implicit character with length
    if (.not. test_implicit_character_with_length()) all_tests_passed = .false.
    
    ! Test implicit type with multiple letter ranges
    if (.not. test_implicit_multiple_ranges()) all_tests_passed = .false.
    
    if (all_tests_passed) then
        print *, "All implicit statement parsing tests passed!"
        stop 0
    else
        print *, "Some implicit statement parsing tests failed!"
        stop 1
    end if

contains

    function test_implicit_none() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index
        character(len=:), allocatable :: code_result
        
        passed = .true.
        
        ! Create arena
        arena = create_ast_arena()
        
        ! Create tokens for "implicit none"
        allocate(tokens(3))
        tokens(1)%kind = TK_KEYWORD
        tokens(1)%text = "implicit"
        tokens(1)%line = 1
        tokens(1)%column = 1
        
        tokens(2)%kind = TK_KEYWORD
        tokens(2)%text = "none"
        tokens(2)%line = 1
        tokens(2)%column = 10
        
        tokens(3)%kind = TK_EOF
        tokens(3)%text = ""
        tokens(3)%line = 1
        tokens(3)%column = 15
        
        ! Create parser
        parser = create_parser_state(tokens)
        
        ! Parse implicit statement
        stmt_index = parse_implicit_statement(parser, arena)
        
        ! Generate code to verify
        code_result = generate_code_from_arena(arena, stmt_index)
        
        ! Check result
        if (trim(code_result) /= "implicit none") then
            passed = .false.
            print *, "FAIL: Expected 'implicit none', got '", trim(code_result), "'"
        end if
        
        if (passed) then
            print *, "PASS: test_implicit_none"
        else
            print *, "FAIL: test_implicit_none"
        end if
        
        call arena%clear()
    end function test_implicit_none
    
    function test_implicit_real_range() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index
        character(len=:), allocatable :: code_result
        
        passed = .true.
        
        ! Create arena
        arena = create_ast_arena()
        
        ! Create tokens for "implicit real (a-h,o-z)"
        allocate(tokens(11))
        tokens(1)%kind = TK_KEYWORD
        tokens(1)%text = "implicit"
        tokens(1)%line = 1
        tokens(1)%column = 1
        
        tokens(2)%kind = TK_KEYWORD
        tokens(2)%text = "real"
        tokens(2)%line = 1
        tokens(2)%column = 10
        
        tokens(3)%kind = TK_OPERATOR
        tokens(3)%text = "("
        tokens(3)%line = 1
        tokens(3)%column = 15
        
        tokens(4)%kind = TK_IDENTIFIER
        tokens(4)%text = "a"
        tokens(4)%line = 1
        tokens(4)%column = 16
        
        tokens(5)%kind = TK_OPERATOR
        tokens(5)%text = "-"
        tokens(5)%line = 1
        tokens(5)%column = 17
        
        tokens(6)%kind = TK_IDENTIFIER
        tokens(6)%text = "h"
        tokens(6)%line = 1
        tokens(6)%column = 18
        
        tokens(7)%kind = TK_OPERATOR
        tokens(7)%text = ","
        tokens(7)%line = 1
        tokens(7)%column = 19
        
        tokens(8)%kind = TK_IDENTIFIER
        tokens(8)%text = "o"
        tokens(8)%line = 1
        tokens(8)%column = 20
        
        tokens(9)%kind = TK_OPERATOR
        tokens(9)%text = "-"
        tokens(9)%line = 1
        tokens(9)%column = 21
        
        tokens(10)%kind = TK_IDENTIFIER
        tokens(10)%text = "z"
        tokens(10)%line = 1
        tokens(10)%column = 22
        
        tokens(11)%kind = TK_OPERATOR
        tokens(11)%text = ")"
        tokens(11)%line = 1
        tokens(11)%column = 23
        
        ! Create parser
        parser = create_parser_state(tokens)
        
        ! Parse implicit statement
        stmt_index = parse_implicit_statement(parser, arena)
        
        ! Generate code to verify
        code_result = generate_code_from_arena(arena, stmt_index)
        
        ! Check result contains expected elements
        if (index(code_result, "implicit") == 0 .or. &
            index(code_result, "real") == 0 .or. &
            index(code_result, "a-h") == 0 .or. &
            index(code_result, "o-z") == 0) then
            passed = .false.
            print *, "FAIL: Expected implicit real statement with ranges, got '", trim(code_result), "'"
        end if
        
        if (passed) then
            print *, "PASS: test_implicit_real_range"
        else
            print *, "FAIL: test_implicit_real_range"
        end if
        
        call arena%clear()
    end function test_implicit_real_range
    
    function test_implicit_integer_range() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index
        character(len=:), allocatable :: code_result
        
        passed = .true.
        
        ! Create arena
        arena = create_ast_arena()
        
        ! Create tokens for "implicit integer (i-n)"
        allocate(tokens(8))
        tokens(1)%kind = TK_KEYWORD
        tokens(1)%text = "implicit"
        tokens(1)%line = 1
        tokens(1)%column = 1
        
        tokens(2)%kind = TK_KEYWORD
        tokens(2)%text = "integer"
        tokens(2)%line = 1
        tokens(2)%column = 10
        
        tokens(3)%kind = TK_OPERATOR
        tokens(3)%text = "("
        tokens(3)%line = 1
        tokens(3)%column = 18
        
        tokens(4)%kind = TK_IDENTIFIER
        tokens(4)%text = "i"
        tokens(4)%line = 1
        tokens(4)%column = 19
        
        tokens(5)%kind = TK_OPERATOR
        tokens(5)%text = "-"
        tokens(5)%line = 1
        tokens(5)%column = 20
        
        tokens(6)%kind = TK_IDENTIFIER
        tokens(6)%text = "n"
        tokens(6)%line = 1
        tokens(6)%column = 21
        
        tokens(7)%kind = TK_OPERATOR
        tokens(7)%text = ")"
        tokens(7)%line = 1
        tokens(7)%column = 22
        
        tokens(8)%kind = TK_EOF
        tokens(8)%text = ""
        tokens(8)%line = 1
        tokens(8)%column = 23
        
        ! Create parser
        parser = create_parser_state(tokens)
        
        ! Parse implicit statement
        stmt_index = parse_implicit_statement(parser, arena)
        
        ! Generate code to verify
        code_result = generate_code_from_arena(arena, stmt_index)
        
        ! Check result
        if (index(code_result, "implicit") == 0 .or. &
            index(code_result, "integer") == 0 .or. &
            index(code_result, "i-n") == 0) then
            passed = .false.
            print *, "FAIL: Expected implicit integer(i-n), got '", trim(code_result), "'"
        end if
        
        if (passed) then
            print *, "PASS: test_implicit_integer_range"
        else
            print *, "FAIL: test_implicit_integer_range"
        end if
        
        call arena%clear()
    end function test_implicit_integer_range
    
    function test_implicit_real_with_kind() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index
        character(len=:), allocatable :: code_result
        
        passed = .true.
        
        ! Create arena
        arena = create_ast_arena()
        
        ! Create tokens for "implicit real(8) (a-h)"
        allocate(tokens(10))
        tokens(1)%kind = TK_KEYWORD
        tokens(1)%text = "implicit"
        tokens(1)%line = 1
        tokens(1)%column = 1
        
        tokens(2)%kind = TK_KEYWORD
        tokens(2)%text = "real"
        tokens(2)%line = 1
        tokens(2)%column = 10
        
        tokens(3)%kind = TK_OPERATOR
        tokens(3)%text = "("
        tokens(3)%line = 1
        tokens(3)%column = 14
        
        tokens(4)%kind = TK_NUMBER
        tokens(4)%text = "8"
        tokens(4)%line = 1
        tokens(4)%column = 15
        
        tokens(5)%kind = TK_OPERATOR
        tokens(5)%text = ")"
        tokens(5)%line = 1
        tokens(5)%column = 16
        
        tokens(6)%kind = TK_OPERATOR
        tokens(6)%text = "("
        tokens(6)%line = 1
        tokens(6)%column = 18
        
        tokens(7)%kind = TK_IDENTIFIER
        tokens(7)%text = "a"
        tokens(7)%line = 1
        tokens(7)%column = 19
        
        tokens(8)%kind = TK_OPERATOR
        tokens(8)%text = "-"
        tokens(8)%line = 1
        tokens(8)%column = 20
        
        tokens(9)%kind = TK_IDENTIFIER
        tokens(9)%text = "h"
        tokens(9)%line = 1
        tokens(9)%column = 21
        
        tokens(10)%kind = TK_OPERATOR
        tokens(10)%text = ")"
        tokens(10)%line = 1
        tokens(10)%column = 22
        
        ! Create parser
        parser = create_parser_state(tokens)
        
        ! Parse implicit statement
        stmt_index = parse_implicit_statement(parser, arena)
        
        ! Generate code to verify
        code_result = generate_code_from_arena(arena, stmt_index)
        
        ! Check result
        if (index(code_result, "implicit") == 0 .or. &
            index(code_result, "real(8)") == 0 .or. &
            index(code_result, "a-h") == 0) then
            passed = .false.
            print *, "FAIL: Expected implicit real(8)(a-h), got '", trim(code_result), "'"
        end if
        
        if (passed) then
            print *, "PASS: test_implicit_real_with_kind"
        else
            print *, "FAIL: test_implicit_real_with_kind"
        end if
        
        call arena%clear()
    end function test_implicit_real_with_kind
    
    function test_implicit_character_with_length() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index
        character(len=:), allocatable :: code_result
        
        passed = .true.
        
        ! Create arena
        arena = create_ast_arena()
        
        ! Create tokens for "implicit character(len=10) (s-z)"
        allocate(tokens(12))
        tokens(1)%kind = TK_KEYWORD
        tokens(1)%text = "implicit"
        tokens(1)%line = 1
        tokens(1)%column = 1
        
        tokens(2)%kind = TK_KEYWORD
        tokens(2)%text = "character"
        tokens(2)%line = 1
        tokens(2)%column = 10
        
        tokens(3)%kind = TK_OPERATOR
        tokens(3)%text = "("
        tokens(3)%line = 1
        tokens(3)%column = 19
        
        tokens(4)%kind = TK_KEYWORD
        tokens(4)%text = "len"
        tokens(4)%line = 1
        tokens(4)%column = 20
        
        tokens(5)%kind = TK_OPERATOR
        tokens(5)%text = "="
        tokens(5)%line = 1
        tokens(5)%column = 23
        
        tokens(6)%kind = TK_NUMBER
        tokens(6)%text = "10"
        tokens(6)%line = 1
        tokens(6)%column = 24
        
        tokens(7)%kind = TK_OPERATOR
        tokens(7)%text = ")"
        tokens(7)%line = 1
        tokens(7)%column = 26
        
        tokens(8)%kind = TK_OPERATOR
        tokens(8)%text = "("
        tokens(8)%line = 1
        tokens(8)%column = 28
        
        tokens(9)%kind = TK_IDENTIFIER
        tokens(9)%text = "s"
        tokens(9)%line = 1
        tokens(9)%column = 29
        
        tokens(10)%kind = TK_OPERATOR
        tokens(10)%text = "-"
        tokens(10)%line = 1
        tokens(10)%column = 30
        
        tokens(11)%kind = TK_IDENTIFIER
        tokens(11)%text = "z"
        tokens(11)%line = 1
        tokens(11)%column = 31
        
        tokens(12)%kind = TK_OPERATOR
        tokens(12)%text = ")"
        tokens(12)%line = 1
        tokens(12)%column = 32
        
        ! Create parser
        parser = create_parser_state(tokens)
        
        ! Parse implicit statement
        stmt_index = parse_implicit_statement(parser, arena)
        
        ! Generate code to verify
        code_result = generate_code_from_arena(arena, stmt_index)
        
        ! Check result
        if (index(code_result, "implicit") == 0 .or. &
            index(code_result, "character") == 0 .or. &
            index(code_result, "len=10") == 0 .or. &
            index(code_result, "s-z") == 0) then
            passed = .false.
            print *, "FAIL: Expected implicit character(len=10)(s-z), got '", trim(code_result), "'"
        end if
        
        if (passed) then
            print *, "PASS: test_implicit_character_with_length"
        else
            print *, "FAIL: test_implicit_character_with_length"
        end if
        
        call arena%clear()
    end function test_implicit_character_with_length
    
    function test_implicit_multiple_ranges() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_index
        character(len=:), allocatable :: code_result
        
        passed = .true.
        
        ! Create arena
        arena = create_ast_arena()
        
        ! Create tokens for "implicit real (a,d-f,z)"
        allocate(tokens(11))
        tokens(1)%kind = TK_KEYWORD
        tokens(1)%text = "implicit"
        tokens(1)%line = 1
        tokens(1)%column = 1
        
        tokens(2)%kind = TK_KEYWORD
        tokens(2)%text = "real"
        tokens(2)%line = 1
        tokens(2)%column = 10
        
        tokens(3)%kind = TK_OPERATOR
        tokens(3)%text = "("
        tokens(3)%line = 1
        tokens(3)%column = 15
        
        tokens(4)%kind = TK_IDENTIFIER
        tokens(4)%text = "a"
        tokens(4)%line = 1
        tokens(4)%column = 16
        
        tokens(5)%kind = TK_OPERATOR
        tokens(5)%text = ","
        tokens(5)%line = 1
        tokens(5)%column = 17
        
        tokens(6)%kind = TK_IDENTIFIER
        tokens(6)%text = "d"
        tokens(6)%line = 1
        tokens(6)%column = 18
        
        tokens(7)%kind = TK_OPERATOR
        tokens(7)%text = "-"
        tokens(7)%line = 1
        tokens(7)%column = 19
        
        tokens(8)%kind = TK_IDENTIFIER
        tokens(8)%text = "f"
        tokens(8)%line = 1
        tokens(8)%column = 20
        
        tokens(9)%kind = TK_OPERATOR
        tokens(9)%text = ","
        tokens(9)%line = 1
        tokens(9)%column = 21
        
        tokens(10)%kind = TK_IDENTIFIER
        tokens(10)%text = "z"
        tokens(10)%line = 1
        tokens(10)%column = 22
        
        tokens(11)%kind = TK_OPERATOR
        tokens(11)%text = ")"
        tokens(11)%line = 1
        tokens(11)%column = 23
        
        ! Create parser
        parser = create_parser_state(tokens)
        
        ! Parse implicit statement
        stmt_index = parse_implicit_statement(parser, arena)
        
        ! Generate code to verify
        code_result = generate_code_from_arena(arena, stmt_index)
        
        ! Check result
        if (index(code_result, "implicit") == 0 .or. &
            index(code_result, "real") == 0 .or. &
            index(code_result, "a") == 0 .or. &
            index(code_result, "d-f") == 0 .or. &
            index(code_result, "z") == 0) then
            passed = .false.
            print *, "FAIL: Expected implicit real(a,d-f,z), got '", trim(code_result), "'"
        end if
        
        if (passed) then
            print *, "PASS: test_implicit_multiple_ranges"
        else
            print *, "FAIL: test_implicit_multiple_ranges"
        end if
        
        call arena%clear()
    end function test_implicit_multiple_ranges

end program test_implicit_statement_parsing