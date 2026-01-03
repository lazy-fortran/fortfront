program test_lfortran_template_instantiation_parsing
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

    call read_example('examples/f90/issue_2737_lfortran_template_instantiate.f90', &
                      source)

    arena = create_ast_arena()
    call lex_source(source, tokens, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Lexing error: " // trim(error_msg)
        error stop 1
    end if

    call parse_tokens(tokens, arena, root_index, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Parsing error: " // trim(error_msg)
        error stop 1
    end if

    call emit_fortran(arena, root_index, output_code)

    if (index(output_code, "template swap") == 0) then
        write (output_unit, '(A)') "FAIL: template block missing from output"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    if (index(output_code, "instantiate swap{integer}") == 0) then
        write (output_unit, '(A)') "FAIL: instantiate statement missing from output"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    if (index(output_code, "call swap{integer}") == 0) then
        write (output_unit, '(A)') &
            "FAIL: inline instantiation missing in call statement"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    if (index(output_code, "identity{") == 0) then
        write (output_unit, '(A)') &
            "FAIL: inline instantiation missing in expression call"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    write (output_unit, '(A)') "PASS: Parsed template/instantiate and inline {}"

contains


    include '../common/read_example.inc'
end program test_lfortran_template_instantiation_parsing
