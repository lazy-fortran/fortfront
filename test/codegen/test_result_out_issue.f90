program test_result_out_issue
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use standardizer, only: standardize_ast
    use codegen_core, only: codegen_core_generate_arena, initialize_codegen
    use ast_arena_modern, only: ast_arena_t
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: error_msg, code, source
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: root_index
    logical :: ok

    ok = .true.

    print *, "=== Testing result(out) Issue ==="
    print *, ""

    source = &
        "program p" // new_line('a') // &
        "contains" // new_line('a') // &
        "function test_func(arr) result(out)" // new_line('a') // &
        "real, dimension(3), intent(in) :: arr" // new_line('a') // &
        "real, dimension(3) :: out" // new_line('a') // &
        "out = arr * 2.0" // new_line('a') // &
        "end function" // new_line('a') // &
        "end program p"

    call initialize_codegen()
    call lex_source(source, tokens, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: lexing error:', trim(error_msg)
        stop 1
    end if

    call parse_tokens(tokens, arena, root_index, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: parsing error:', trim(error_msg)
        stop 1
    end if

    call standardize_ast(arena, root_index)

    code = codegen_core_generate_arena(arena, root_index)

    print *, "Generated code:"
    print *, code
    print *, ""

    if (index(code, "out = arr") <= 0) then
        print *, "FAIL: Body statement missing"
        ok = .false.
    else
        print *, "PASS: Body statement found"
    end if

    if (index(code, "result(out)") <= 0) then
        print *, "FAIL: result(out) not found"
        ok = .false.
    else
        print *, "PASS: result(out) found"
    end if

    if (ok) then
        print *, ""
        print *, "All tests PASSED"
    else
        print *, ""
        print *, "Some tests FAILED"
        stop 1
    end if

end program test_result_out_issue
