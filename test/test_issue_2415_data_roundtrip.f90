program test_issue_2415_data_roundtrip
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: source, first_output, second_output
    character(len=:), allocatable :: error_msg
    type(ast_arena_t) :: arena1, arena2
    type(token_t), allocatable :: tokens1(:), tokens2(:)
    integer :: root1, root2
    logical :: test_passed

    test_passed = .true.

    call read_example('examples/f90/data_stmt_parse.f90', source)

    arena1 = create_ast_arena()
    call lex_source(source, tokens1, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: First lex error: " // trim(error_msg)
        error stop 1
    end if

    call parse_tokens(tokens1, arena1, root1, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: First parse error: " // trim(error_msg)
        error stop 1
    end if

    call emit_fortran(arena1, root1, first_output)
    write (output_unit, '(A)') "First pass output:"
    write (output_unit, '(A)') first_output

    arena2 = create_ast_arena()
    call lex_source(first_output, tokens2, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Second lex error: " // trim(error_msg)
        write (output_unit, '(A)') "Output was:"
        write (output_unit, '(A)') first_output
        error stop 1
    end if

    call parse_tokens(tokens2, arena2, root2, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Second parse error: " // trim(error_msg)
        write (output_unit, '(A)') "This is the round-trip bug - emitted code "// &
            "cannot be parsed"
        write (output_unit, '(A)') "Output was:"
        write (output_unit, '(A)') first_output
        error stop 1
    end if

    call emit_fortran(arena2, root2, second_output)

    write (output_unit, '(A)') "PASS: DATA statement round-trip succeeded"


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2415_data_roundtrip
