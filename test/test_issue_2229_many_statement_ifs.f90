program test_issue_2229_many_statement_ifs
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    character(:), allocatable :: input_code, output_code, error_msg
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index

    print *, "=== Issue #2229: many statement IFs should parse efficiently ==="

    call read_example('examples/f90/issue_2229_many_statement_ifs.f90', input_code)

    ! This should complete quickly (not take minutes)
    arena = create_ast_arena()
    call lex_source(input_code, tokens, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "FAIL: Lexing failed:", error_msg
        error stop 1
    end if

    call parse_tokens(tokens, arena, prog_index, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "FAIL: Parsing failed:", error_msg
        error stop 1
    end if

    call emit_fortran(arena, prog_index, output_code)

    ! Verify the output contains the expected structure
    if (index(output_code, "program many_statement_ifs") == 0) then
        print *, "FAIL: Missing program declaration"
        error stop 1
    end if

    if (index(output_code, "if (acc == 0) acc = acc + 1") == 0) then
        print *, "FAIL: Missing first IF statement"
        error stop 1
    end if

    if (index(output_code, "if (acc == 99) acc = acc + 1") == 0) then
        print *, "FAIL: Missing last IF statement"
        error stop 1
    end if

    print *, "PASS: many statement IFs parsed efficiently"


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2229_many_statement_ifs
