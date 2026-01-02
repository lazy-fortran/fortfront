program test_issue_2401_data_stmt_parse
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: source, output_code, error_msg
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: root_index
    logical :: test_passed

    test_passed = .true.

    call read_example('examples/f90/data_stmt_parse.f90', source)

    arena = create_ast_arena()
    call lex_source(source, tokens, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Lexing error: " // trim(error_msg)
        error stop 1
    end if

    call parse_tokens(tokens, arena, root_index, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Parsing error: " // trim(error_msg)
        write (output_unit, '(A)') "Error was: " // error_msg
        error stop 1
    end if

    call emit_fortran(arena, root_index, output_code)

    if (index(output_code, "data") == 0) then
        write (output_unit, '(A)') "FAIL: data keyword missing from output"
        write (output_unit, '(A)') "Output was:"
        write (output_unit, '(A)') output_code
        test_passed = .false.
    end if

    if (index(output_code, "integer") == 0 .and. index(output_code, "dimension") == 0) then
        write (output_unit, '(A)') "FAIL: array declaration missing from output"
        test_passed = .false.
    end if

    if (index(output_code, "print") == 0) then
        write (output_unit, '(A)') "FAIL: print statement missing from output"
        test_passed = .false.
    end if

    if (test_passed) then
        write (output_unit, '(A)') "PASS: Issue #2401 data statement parsed correctly"
    else
        error stop 1
    end if


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2401_data_stmt_parse
