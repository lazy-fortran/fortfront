program test_frontend_lexer_edge_cases
    use lexer_core, only: TK_IDENTIFIER, TK_NEWLINE, TK_NUMBER, &
        TK_OPERATOR, TK_STRING, TK_COMMENT, TK_WHITESPACE, &
        TK_UNKNOWN, token_t, tokenize_core, &
        tokenize_core_with_trivia, tokenize_result_t, &
        tokenize_safe
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. test_continuation_and_positions()) all_passed = .false.
    if (.not. test_continuation_comment_positions()) all_passed = .false.
    if (.not. test_continued_character_literals()) all_passed = .false.
    if (.not. test_kind_parameter_literals()) all_passed = .false.
    if (.not. test_complex_and_hollerith_literals()) all_passed = .false.
    if (.not. test_trivia_preservation()) all_passed = .false.
    if (.not. test_error_recovery_cases()) all_passed = .false.

    if (all_passed) then
        print '(a)', "All lexer edge case tests passed"
        stop 0
    end if

    print '(a)', "Some lexer edge case tests failed"
    stop 1

contains

    logical function test_continuation_comment_positions()
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: source
        integer :: amp_index, comment_index, value_index

        test_continuation_comment_positions = .true.
        source = "case (1, &"//new_line('a')// &
            "    ! full-line comment"//new_line('a')//"    2)"
        call tokenize_core(source, tokens)

        amp_index = find_token_text(tokens, "&")
        comment_index = find_token_text(tokens, "! full-line comment")
        value_index = find_token_text(tokens, "2")

        if (amp_index == 0 .or. .not. expect_position(tokens, amp_index, 1, 10)) then
            print '(a)', "FAIL: continuation ampersand position changed"
            test_continuation_comment_positions = .false.
        end if
        if (comment_index == 0 .or. .not. expect_position(tokens, comment_index, 2, 5)) then
            print '(a)', "FAIL: full-line comment trivia position changed"
            test_continuation_comment_positions = .false.
        end if
        if (value_index == 0 .or. .not. expect_position(tokens, value_index, 3, 5)) then
            print '(a)', "FAIL: resumed token position changed"
            test_continuation_comment_positions = .false.
        end if

        call tokenize_core_with_trivia(source, tokens)
        value_index = find_token_text(tokens, "2")
        if (value_index == 0 .or. .not. has_leading_trivia_text(tokens(value_index), &
            "! full-line comment")) then
            print '(a)', "FAIL: continuation comment trivia was not attached"
            test_continuation_comment_positions = .false.
        end if

        source = "text = '! marker' // &"//new_line('a')// &
            "    ! comment containing '!'"//new_line('a')// &
            "    & '! still literal'"
        call tokenize_core(source, tokens)
        if (.not. has_token_text(tokens, "'! marker'")) then
            print '(a)', "FAIL: ! inside first character literal was not literal"
            test_continuation_comment_positions = .false.
        end if
        if (.not. has_token_text(tokens, "'! still literal'")) then
            print '(a)', "FAIL: ! inside resumed character literal was not literal"
            test_continuation_comment_positions = .false.
        end if
        if (.not. has_token_text(tokens, "! comment containing '!'")) then
            print '(a)', "FAIL: comment trivia was not kept separate"
            test_continuation_comment_positions = .false.
        end if
    end function test_continuation_comment_positions

    logical function test_continuation_and_positions()
        type(token_t), allocatable :: tokens(:)

        test_continuation_and_positions = .true.

        call tokenize_core("x=&"//new_line('a')//"y=2", tokens)

        if (.not. expect_token(tokens, 3, TK_OPERATOR, "&")) then
            test_continuation_and_positions = .false.
        end if
        if (.not. expect_position(tokens, 3, 1, 3)) then
            test_continuation_and_positions = .false.
        end if
        if (.not. expect_token(tokens, 4, TK_NEWLINE, new_line('a'))) then
            test_continuation_and_positions = .false.
        end if
        if (.not. expect_position(tokens, 5, 2, 1)) then
            test_continuation_and_positions = .false.
        end if
    end function test_continuation_and_positions

    logical function test_continued_character_literals()
        type(token_t), allocatable :: tokens(:)

        test_continued_character_literals = .true.

        call tokenize_core("'abc   &"//new_line('a')//"    &def'", tokens)
        if (.not. expect_token(tokens, 1, TK_STRING, "'abc   def'")) then
            test_continued_character_literals = .false.
        end if

        ! GNU Fortran accepts the widely used omitted leading ampersand as an
        ! extension. FortFront accepts it too so valid downstream corpora are
        ! not dropped from builds.
        call tokenize_core("'abc &"//new_line('a')//"    def'", tokens)
        if (.not. expect_token(tokens, 1, TK_STRING, "'abc def'")) then
            test_continued_character_literals = .false.
        end if
    end function test_continued_character_literals

    logical function test_kind_parameter_literals()
        type(token_t), allocatable :: tokens(:)

        test_kind_parameter_literals = .true.

        call tokenize_core("1_int32 1.0_real64 1.0e-3_dp", tokens)

        if (.not. expect_token(tokens, 1, TK_NUMBER, "1_int32")) then
            test_kind_parameter_literals = .false.
        end if
        if (.not. expect_token(tokens, 2, TK_NUMBER, "1.0_real64")) then
            test_kind_parameter_literals = .false.
        end if
        if (.not. expect_token(tokens, 3, TK_NUMBER, "1.0e-3_dp")) then
            test_kind_parameter_literals = .false.
        end if
    end function test_kind_parameter_literals

    logical function test_complex_and_hollerith_literals()
        type(token_t), allocatable :: tokens(:)

        test_complex_and_hollerith_literals = .true.

        call tokenize_core("(1.0, 2.0)", tokens)

        if (.not. expect_token(tokens, 1, TK_OPERATOR, "(")) then
            test_complex_and_hollerith_literals = .false.
        end if
        if (.not. expect_token(tokens, 2, TK_NUMBER, "1.0")) then
            test_complex_and_hollerith_literals = .false.
        end if
        if (.not. expect_token(tokens, 3, TK_OPERATOR, ",")) then
            test_complex_and_hollerith_literals = .false.
        end if
        if (.not. expect_token(tokens, 4, TK_NUMBER, "2.0")) then
            test_complex_and_hollerith_literals = .false.
        end if
        if (.not. expect_token(tokens, 5, TK_OPERATOR, ")")) then
            test_complex_and_hollerith_literals = .false.
        end if

        call tokenize_core("2Hab", tokens)
        if (.not. expect_token(tokens, 1, TK_STRING, "2Hab")) then
            test_complex_and_hollerith_literals = .false.
        end if
    end function test_complex_and_hollerith_literals

    logical function test_trivia_preservation()
        type(token_t), allocatable :: tokens(:)
        integer :: x_index, number_index

        test_trivia_preservation = .true.

        call tokenize_core_with_trivia("  x = 1 ! tail", tokens)

        x_index = find_token_text(tokens, "x")
        number_index = find_token_text(tokens, "1")

        if (x_index == 0 .or. number_index == 0) then
            print '(a)', "FAIL: expected tokens missing in trivia test"
            test_trivia_preservation = .false.
            return
        end if

        if (.not. allocated(tokens(x_index)%leading_trivia)) then
            print '(a)', "FAIL: leading trivia missing before identifier"
            test_trivia_preservation = .false.
        else if (tokens(x_index)%leading_trivia(1)%kind /= TK_WHITESPACE .or. &
                tokens(x_index)%leading_trivia(1)%text /= "  ") then
            print '(a)', "FAIL: leading whitespace trivia not preserved"
            test_trivia_preservation = .false.
        end if

        if (.not. allocated(tokens(number_index)%trailing_trivia)) then
            print '(a)', "FAIL: trailing trivia missing after number"
            test_trivia_preservation = .false.
        else if (.not. has_trivia_kind(tokens(number_index), TK_COMMENT)) then
            print '(a)', "FAIL: trailing comment trivia not preserved"
            test_trivia_preservation = .false.
        end if
    end function test_trivia_preservation

    logical function test_error_recovery_cases()
        type(tokenize_result_t) :: result

        test_error_recovery_cases = .true.

        result = tokenize_safe("'unterminated")
        if (.not. result%success) then
            print '(a)', "FAIL: safe tokenization rejected unterminated string"
            test_error_recovery_cases = .false.
        end if
        if (.not. expect_token(result%tokens, 1, TK_STRING, "'unterminated'")) then
            test_error_recovery_cases = .false.
        end if

        result = tokenize_safe("x @ y")
        if (.not. result%success) then
            print '(a)', "FAIL: safe tokenization rejected invalid character"
            test_error_recovery_cases = .false.
        end if
        if (.not. expect_token(result%tokens, 1, TK_IDENTIFIER, "x")) then
            test_error_recovery_cases = .false.
        end if
        if (.not. expect_token(result%tokens, 2, TK_IDENTIFIER, "y")) then
            test_error_recovery_cases = .false.
        end if
        if (has_token_kind(result%tokens, TK_UNKNOWN)) then
            print '(a)', "FAIL: invalid character produced unknown token"
            test_error_recovery_cases = .false.
        end if
    end function test_error_recovery_cases

    logical function expect_token(tokens, index, kind, text)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: index, kind
        character(len=*), intent(in) :: text

        expect_token = .true.

        if (index > size(tokens)) then
            print '(a,i0)', "FAIL: missing token at index ", index
            expect_token = .false.
            return
        end if

        if (tokens(index)%kind /= kind) then
            print '(a,i0,a,i0)', "FAIL: token kind at index ", index, &
                " was ", tokens(index)%kind
            expect_token = .false.
        end if
        if (.not. allocated(tokens(index)%text)) then
            print '(a,i0)', "FAIL: token text not allocated at index ", index
            expect_token = .false.
        else if (tokens(index)%text /= text) then
            print '(a,i0,a,a,a)', "FAIL: token text at index ", index, &
                " was '", tokens(index)%text, "'"
            expect_token = .false.
        end if
    end function expect_token

    logical function expect_position(tokens, index, line, column)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: index, line, column

        expect_position = .true.

        if (index > size(tokens)) then
            print '(a,i0)', "FAIL: missing positioned token at index ", index
            expect_position = .false.
            return
        end if

        if (tokens(index)%line /= line .or. tokens(index)%column /= column) then
            print '(a,i0,a,i0,a,i0)', "FAIL: token position at index ", &
                index, " was line ", tokens(index)%line, " column ", &
                tokens(index)%column
            expect_position = .false.
        end if
    end function expect_position

    integer function find_token_text(tokens, text)
        type(token_t), intent(in) :: tokens(:)
        character(len=*), intent(in) :: text
        integer :: i

        find_token_text = 0

        do i = 1, size(tokens)
            if (allocated(tokens(i)%text)) then
                if (tokens(i)%text == text) then
                    find_token_text = i
                    return
                end if
            end if
        end do
    end function find_token_text

    logical function has_token_text(tokens, text)
        type(token_t), intent(in) :: tokens(:)
        character(len=*), intent(in) :: text

        has_token_text = find_token_text(tokens, text) > 0
    end function has_token_text

    logical function has_trivia_kind(token, kind)
        type(token_t), intent(in) :: token
        integer, intent(in) :: kind
        integer :: i

        has_trivia_kind = .false.

        if (.not. allocated(token%trailing_trivia)) return

        do i = 1, size(token%trailing_trivia)
            if (token%trailing_trivia(i)%kind == kind) then
                has_trivia_kind = .true.
                return
            end if
        end do
    end function has_trivia_kind

    logical function has_leading_trivia_text(token, text)
        type(token_t), intent(in) :: token
        character(len=*), intent(in) :: text
        integer :: i

        has_leading_trivia_text = .false.
        if (.not. allocated(token%leading_trivia)) return
        do i = 1, size(token%leading_trivia)
            if (.not. allocated(token%leading_trivia(i)%text)) cycle
            if (token%leading_trivia(i)%text == text) then
                has_leading_trivia_text = .true.
                return
            end if
        end do
    end function has_leading_trivia_text

    logical function has_token_kind(tokens, kind)
        type(token_t), intent(in) :: tokens(:)
        integer, intent(in) :: kind
        integer :: i

        has_token_kind = .false.

        do i = 1, size(tokens)
            if (tokens(i)%kind == kind) then
                has_token_kind = .true.
                return
            end if
        end do
    end function has_token_kind

end program test_frontend_lexer_edge_cases
