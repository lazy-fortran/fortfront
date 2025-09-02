program test_boolean_precedence
    use frontend, only: lex_source, parse_tokens, emit_fortran
    use ast_core, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed
    all_passed = .true.

    print *, '=== Testing Boolean NOT precedence and parentheses ==='

    if (.not. test_not_over_and_parentheses()) all_passed = .false.

    if (all_passed) then
        print *, 'All boolean precedence tests passed!'
        stop 0
    else
        print *, 'Some boolean precedence tests failed!'
        stop 1
    end if

contains

    function test_not_over_and_parentheses() result(passed)
        logical :: passed
        character(len=:), allocatable :: out
        integer :: pos_not, pos_paren_rel, pos_paren, pos_and, pos_close_rel

        out = compile_and_generate('result = .not. (.true. .and. .false.)')

        passed = .false.

        ! Basic sanity: contains .not. and .and.
        if (index(out, '.not.') == 0 .or. index(out, '.and.') == 0) then
            print *, 'FAIL: output missing logical operators'
            return
        end if

        ! Expect parentheses immediately after .not. to preserve grouping
        pos_not = index(out, '.not.')
        if (pos_not == 0) then
            print *, 'FAIL: .not. not found in output'
            return
        end if
        pos_paren_rel = index(out(pos_not:), '(')
        if (pos_paren_rel > 0) then
            pos_paren = pos_not + pos_paren_rel - 1
        else
            pos_paren = 0
        end if
        pos_and = index(out, '.and.')

        if (pos_paren == 0 .or. pos_paren <= pos_not) then
            print *, 'FAIL: missing parentheses after .not. for grouped expression'
            return
        end if

        if (pos_and == 0 .or. pos_and < pos_paren) then
            print *, 'FAIL: expected .and. within parenthesized .not. expression'
            return
        end if

        pos_close_rel = index(out(pos_and:), ')')
        if (pos_close_rel == 0) then
            print *, 'FAIL: missing closing parenthesis after .and. expression'
            return
        end if

        print *, 'PASS: .not. over (.true. .and. .false.) preserves parentheses'
        passed = .true.
    end function test_not_over_and_parentheses

    function compile_and_generate(source_line) result(output)
        character(len=*), intent(in) :: source_line
        character(len=:), allocatable :: output

        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        source = source_line // new_line('a')

        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, 'Tokenization error: ', error_msg
            output = ''
            return
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, 'Parsing error: ', error_msg
            output = ''
            return
        end if

        call emit_fortran(arena, prog_index, output)
        if (.not. allocated(output)) output = ''
    end function compile_and_generate

end program test_boolean_precedence
