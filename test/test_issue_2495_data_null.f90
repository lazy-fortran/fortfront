program test_issue_2495_data_null
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

    call read_example('examples/f90/issue_2495_data_null_intrinsic.f90', source)

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

    if (index(output_code, "null()") == 0) then
        write (output_unit, '(A)') "FAIL: null() missing from output"
        test_passed = .false.
    end if

    if (index(output_code, "external :: null") > 0) then
        write (output_unit, '(A)') "FAIL: null incorrectly declared as external"
        test_passed = .false.
    end if

    if (test_passed) then
        write (output_unit, '(A)') "PASS: Issue #2495 NULL() recognized as intrinsic"
    else
        write (output_unit, '(A)') "Output was:"
        write (output_unit, '(A)') output_code
        error stop 1
    end if


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2495_data_null
