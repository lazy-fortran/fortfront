program test_unbalanced_inline_instantiation_diagnostics
    use, intrinsic :: iso_fortran_env, only: output_unit
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    call assert_source_error( &
        "program issue_2743_unbalanced_inline_instantiation_call"//new_line('a')// &
        "    implicit none"//new_line('a')// &
        "    call foo{integer"//new_line('a')// &
        "end program issue_2743_unbalanced_inline_instantiation_call", &
        'line 3, column 13')

    call assert_source_error( &
        "program issue_2743_unbalanced_inline_instantiation_expr"//new_line('a')// &
        "    implicit none"//new_line('a')// &
        "    integer :: x"//new_line('a')// &
        "    x = identity{integer"//new_line('a')// &
        "end program issue_2743_unbalanced_inline_instantiation_expr", &
        'line 4, column 17')

    write (output_unit, '(A)') "PASS: Unbalanced inline instantiation diagnostics"

contains

    subroutine assert_source_error(source, expected_location)
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: expected_location

        character(len=:), allocatable :: lex_error
        character(len=5000) :: parse_error
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: root_index

        arena = create_ast_arena()
        call lex_source(source, tokens, lex_error)

        if (allocated(lex_error)) then
            if (len_trim(lex_error) > 0) then
                write (output_unit, '(A)') "FAIL: Lexing error: " // trim(lex_error)
                error stop 1
            end if
        end if

        call parse_tokens(tokens, arena, root_index, parse_error)

        if (len_trim(parse_error) == 0) then
            write (output_unit, '(A)') "FAIL: Expected parse error, got success"
            error stop 1
        end if

        if (index(parse_error, "Unbalanced inline instantiation braces") == 0) then
            write (output_unit, '(A)') "FAIL: Missing unbalanced-braces diagnostic"
            write (output_unit, '(A)') trim(parse_error)
            error stop 1
        end if

        if (index(parse_error, expected_location) == 0) then
            write (output_unit, '(A)') "FAIL: Diagnostic location mismatch"
            write (output_unit, '(A)') "FAIL: Expected: " // trim(expected_location)
            write (output_unit, '(A)') trim(parse_error)
            error stop 1
        end if
    end subroutine assert_source_error
end program test_unbalanced_inline_instantiation_diagnostics
