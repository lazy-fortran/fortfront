program test_inline_instantiation_caret
    use, intrinsic :: iso_fortran_env, only: output_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: source, output_code, error_msg
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: root_index

    call read_example('examples/f90/issue_2817_inline_instantiate_caret.f90', &
        source)

    arena = create_ast_arena()
    call lex_source(source, tokens, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Lexing error: "//trim(error_msg)
        error stop 1
    end if

    call parse_tokens(tokens, arena, root_index, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Parsing error: "//trim(error_msg)
        error stop 1
    end if

    call emit_fortran(arena, root_index, output_code)

    ! Caret inline instantiation in a subroutine call statement.
    if (index(output_code, "call swap^(integer)") == 0) then
        write (output_unit, '(A)') &
            "FAIL: caret inline instantiation missing in call statement"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    ! Caret inline instantiation in a function call (expression position).
    if (index(output_code, "mysum^(integer)") == 0) then
        write (output_unit, '(A)') &
            "FAIL: caret inline instantiation missing in expression call"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    if (index(output_code, "mysum^(real)") == 0) then
        write (output_unit, '(A)') &
            "FAIL: caret real instantiation missing in expression call"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    write (output_unit, '(A)') "PASS: Parsed caret inline instantiation ^()"

contains

    include '../common/read_example.inc'
end program test_inline_instantiation_caret
