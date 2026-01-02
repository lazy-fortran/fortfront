program test_issue_1814_nested_lazy_function
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_nested_function_error_message( &
        'examples/lf/issue_1814_nested_lazy_function.lf')
    call test_nested_function_error_message( &
        'examples/lf/issue_playtest5_nested_function_chaos.lf')
    print *, 'PASS: nested lazy functions emit diagnostics and halt parsing'

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'


    subroutine test_nested_function_error_message(example_path)
        character(len=*), intent(in) :: example_path
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: lex_error
        character(len=500) :: parse_error
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        call read_example(example_path, input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, lex_error)
        if (allocated(lex_error) .and. len_trim(lex_error) > 0) then
            write (error_unit, '(A)') 'Lexing error: ' // trim(lex_error)
            error stop 1
        end if

        parse_error = ''
        call parse_tokens(tokens, arena, prog_index, parse_error)
        if (len_trim(parse_error) == 0) then
            write (error_unit, '(A)') 'FAIL: nested functions did not raise error'
            error stop 1
        end if
        if (prog_index /= 0) then
            write (error_unit, '(A)') 'FAIL: parser returned AST despite error'
            error stop 1
        end if
        if (index(parse_error, 'Nested internal procedures are not supported.') == 0) then
            write (error_unit, '(A)') 'FAIL: diagnostic missing nested procedure text'
            write (error_unit, '(A)') trim(parse_error)
            error stop 1
        end if
    end subroutine test_nested_function_error_message

end program test_issue_1814_nested_lazy_function
