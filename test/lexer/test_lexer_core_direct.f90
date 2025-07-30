program test_lexer_core_direct
    use lexer_core, only: tokenize_core, token_t, token_type_name, &
                          TK_IDENTIFIER, TK_NUMBER, TK_STRING, TK_OPERATOR, &
                          TK_KEYWORD, TK_EOF
    implicit none

    integer :: total_tests, passed_tests
    type(token_t), allocatable :: tokens(:)

    total_tests = 0
    passed_tests = 0

    print *, "=== Lexer Core Direct Function Tests ==="
    print *, ""

    ! Test 1: Empty string tokenization
    call test_start("Empty string tokenization")
    call tokenize_core("", tokens)
    if (size(tokens) == 1 .and. tokens(1)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: 1 EOF token, got:", size(tokens), "tokens"
    end if

    ! Test 2: Single identifier
    call test_start("Single identifier")
    call tokenize_core("hello", tokens)
    if (size(tokens) == 2 .and. tokens(1)%kind == TK_IDENTIFIER .and. &
        tokens(1)%text == "hello" .and. tokens(2)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: identifier 'hello' + EOF"
    end if

    ! Test 3: Single number
    call test_start("Single number")
    call tokenize_core("42", tokens)
    if (size(tokens) == 2 .and. tokens(1)%kind == TK_NUMBER .and. &
        tokens(1)%text == "42" .and. tokens(2)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: number '42' + EOF"
    end if

    ! Test 4: Float number with decimal
    call test_start("Float number with decimal")
    call tokenize_core("3.14", tokens)
    if (size(tokens) == 2 .and. tokens(1)%kind == TK_NUMBER .and. &
        tokens(1)%text == "3.14" .and. tokens(2)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: number '3.14' + EOF"
    end if

    ! Test 5: Scientific notation
    call test_start("Scientific notation")
    call tokenize_core("1.23e-4", tokens)
    if (size(tokens) == 2 .and. tokens(1)%kind == TK_NUMBER .and. &
        tokens(1)%text == "1.23e-4" .and. tokens(2)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: number '1.23e-4' + EOF"
    end if

    ! Test 6: String literal double quotes
    call test_start("String literal double quotes")
    call tokenize_core('"hello world"', tokens)
    if (size(tokens) == 2 .and. tokens(1)%kind == TK_STRING .and. &
        tokens(1)%text == '"hello world"' .and. tokens(2)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, '  Expected: string "hello world" + EOF'
    end if

    ! Test 7: String literal single quotes
    call test_start("String literal single quotes")
    call tokenize_core("'hello world'", tokens)
    if (size(tokens) == 2 .and. tokens(1)%kind == TK_STRING .and. &
        tokens(1)%text == "'hello world'" .and. tokens(2)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: string 'hello world' + EOF"
    end if

    ! Test 8: Keywords recognition
    call test_start("Keyword recognition")
    call tokenize_core("program", tokens)
    if (size(tokens) == 2 .and. tokens(1)%kind == TK_KEYWORD .and. &
        tokens(1)%text == "program" .and. tokens(2)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: keyword 'program' + EOF"
    end if

    ! Test 9: Multiple keywords
    call test_start("Multiple keywords")
    call tokenize_core("if then else", tokens)
    if (size(tokens) == 4 .and. &
        tokens(1)%kind == TK_KEYWORD .and. tokens(1)%text == "if" .and. &
        tokens(2)%kind == TK_KEYWORD .and. tokens(2)%text == "then" .and. &
        tokens(3)%kind == TK_KEYWORD .and. tokens(3)%text == "else" .and. &
        tokens(4)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: 3 keywords + EOF"
    end if

    ! Test 10: Operators
    call test_start("Single operators")
    call tokenize_core("+ - * /", tokens)
    if (size(tokens) == 5 .and. &
        tokens(1)%kind == TK_OPERATOR .and. tokens(1)%text == "+" .and. &
        tokens(2)%kind == TK_OPERATOR .and. tokens(2)%text == "-" .and. &
        tokens(3)%kind == TK_OPERATOR .and. tokens(3)%text == "*" .and. &
        tokens(4)%kind == TK_OPERATOR .and. tokens(4)%text == "/" .and. &
        tokens(5)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: 4 operators + EOF"
    end if

    ! Test 11: Two-character operators
    call test_start("Two-character operators")
    call tokenize_core("== /= <= >=", tokens)
    if (size(tokens) == 5 .and. &
        tokens(1)%kind == TK_OPERATOR .and. tokens(1)%text == "==" .and. &
        tokens(2)%kind == TK_OPERATOR .and. tokens(2)%text == "/=" .and. &
        tokens(3)%kind == TK_OPERATOR .and. tokens(3)%text == "<=" .and. &
        tokens(4)%kind == TK_OPERATOR .and. tokens(4)%text == ">=" .and. &
        tokens(5)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: 4 two-char operators + EOF"
    end if

    ! Test 12: Logical constants
    call test_start("Logical constants")
    call tokenize_core(".true. .false.", tokens)
    if (size(tokens) == 3 .and. &
        tokens(1)%kind == TK_KEYWORD .and. tokens(1)%text == ".true." .and. &
        tokens(2)%kind == TK_KEYWORD .and. tokens(2)%text == ".false." .and. &
        tokens(3)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: 2 logical constants + EOF"
    end if

    ! Test 13: Logical operators
    call test_start("Logical operators")
    call tokenize_core(".and. .or. .not.", tokens)
    if (size(tokens) == 4 .and. &
        tokens(1)%kind == TK_OPERATOR .and. tokens(1)%text == ".and." .and. &
        tokens(2)%kind == TK_OPERATOR .and. tokens(2)%text == ".or." .and. &
        tokens(3)%kind == TK_OPERATOR .and. tokens(3)%text == ".not." .and. &
        tokens(4)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: 3 logical operators + EOF"
    end if

    ! Test 14: Mixed token types
    call test_start("Mixed token types")
    call tokenize_core("x = 42", tokens)
    if (size(tokens) == 4 .and. &
        tokens(1)%kind == TK_IDENTIFIER .and. tokens(1)%text == "x" .and. &
        tokens(2)%kind == TK_OPERATOR .and. tokens(2)%text == "=" .and. &
        tokens(3)%kind == TK_NUMBER .and. tokens(3)%text == "42" .and. &
        tokens(4)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: identifier, operator, number + EOF"
    end if

    ! Test 15: Line and column tracking
    call test_start("Line and column tracking")
    call tokenize_core("a" // new_line('A') // "b", tokens)
    if (size(tokens) == 3 .and. &
        tokens(1)%kind == TK_IDENTIFIER .and. tokens(1)%line == 1 .and. &
        tokens(1)%column == 1 .and. &
        tokens(2)%kind == TK_IDENTIFIER .and. tokens(2)%line == 2 .and. &
        tokens(2)%column == 1 .and. &
        tokens(3)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: line/column tracking across newlines"
    end if

    ! Test 16: Comment handling
    call test_start("Comment handling")
    call tokenize_core("x = 5 ! this is a comment", tokens)
    if (size(tokens) == 4 .and. &
        tokens(1)%kind == TK_IDENTIFIER .and. tokens(1)%text == "x" .and. &
        tokens(2)%kind == TK_OPERATOR .and. tokens(2)%text == "=" .and. &
        tokens(3)%kind == TK_NUMBER .and. tokens(3)%text == "5" .and. &
        tokens(4)%kind == TK_EOF) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: comments to be ignored"
    end if

    ! Test 17: Token type name function
    call test_start("Token type names")
    if (token_type_name(TK_IDENTIFIER) == "identifier" .and. &
        token_type_name(TK_NUMBER) == "number" .and. &
        token_type_name(TK_STRING) == "string" .and. &
        token_type_name(TK_OPERATOR) == "operator" .and. &
        token_type_name(TK_KEYWORD) == "keyword" .and. &
        token_type_name(TK_EOF) == "eof") then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: correct token type names"
    end if

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All lexer core direct tests passed!"
        stop 0
    else
        print *, "Some lexer core tests failed!"
        stop 1
    end if

contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_lexer_core_direct