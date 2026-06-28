program test_crlf_tokenization
    use lexer_core, only: tokenize_core, token_t, TK_KEYWORD, TK_IDENTIFIER
    use fortfront, only: tooling_parse_options_t, tooling_load_ast_from_string, &
        ast_arena_t, ast_to_json
    implicit none

    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    type(tooling_parse_options_t) :: options
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: json_output
    character(len=:), allocatable :: input
    integer :: i
    logical :: found_then, found_sum_neg, found_sum_pos

    input = &
        "program main" // char(13) // char(10) // &
        "    implicit none" // char(13) // char(10) // &
        "    integer :: a(6)" // char(13) // char(10) // &
        "    sum_neg = 0" // char(13) // char(10) // &
        "    sum_pos = 0" // char(13) // char(10) // &
        "    do i = 1, 6" // char(13) // char(10) // &
        "        if (a(i) < 0) then" // char(13) // char(10) // &
        "            sum_neg = sum_neg + a(i)" // char(13) // char(10) // &
        "        else" // char(13) // char(10) // &
        "            sum_pos = sum_pos + a(i)" // char(13) // char(10) // &
        "        end if" // char(13) // char(10) // &
        "    end do" // char(13) // char(10) // &
        "end program main" // char(13) // char(10)

    call tokenize_core(input, tokens)

    found_then = .false.
    found_sum_neg = .false.
    found_sum_pos = .false.

    do i = 1, size(tokens)
        select case (tokens(i)%kind)
        case (TK_KEYWORD)
            if (tokens(i)%text == "then") found_then = .true.
        case (TK_IDENTIFIER)
            if (tokens(i)%text == "sum_neg") found_sum_neg = .true.
            if (tokens(i)%text == "sum_pos") found_sum_pos = .true.
        end select
    end do

    if (.not. found_then) then
        print *, "FAIL: 'then' keyword not recognized for CRLF input"
        stop 1
    end if

    if (.not. found_sum_neg) then
        print *, "FAIL: identifier 'sum_neg' missing for CRLF input"
        stop 1
    end if

    if (.not. found_sum_pos) then
        print *, "FAIL: identifier 'sum_pos' missing for CRLF input"
        stop 1
    end if

    print *, "PASS: CRLF tokenization preserves keywords and identifiers"

    options = tooling_parse_options_t()
    options%run_semantics = .false.
    call tooling_load_ast_from_string(input, arena, i, error_msg, options)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: tooling_load_ast_from_string error:"
        print *, trim(error_msg)
        stop 1
    end if

    call ast_to_json(arena, i, json_output)
    print *, "AST JSON:", trim(json_output)

    if (index(json_output, "sum_neg") == 0) then
        print *, "FAIL: AST JSON missing assignment for 'sum_neg'"
        stop 1
    end if

    if (index(json_output, "sum_pos") == 0) then
        print *, "FAIL: AST JSON missing assignment for 'sum_pos'"
        stop 1
    end if

    print *, "PASS: CRLF AST retains assignments in if branches"
end program test_crlf_tokenization
