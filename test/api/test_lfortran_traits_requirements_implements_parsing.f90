program test_lfortran_traits_requirements_implements_parsing
    use, intrinsic :: iso_fortran_env, only: output_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: root_index

    call read_example( &
        'examples/f90/issue_2738_lfortran_traits_requirements_implements.f90', source)

    arena = create_ast_arena()
    call lex_source(source, tokens, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (output_unit, '(A)') "FAIL: Lexing error: " // trim(error_msg)
            error stop 1
        end if
    end if

    call parse_tokens(tokens, arena, root_index, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (output_unit, '(A)') "FAIL: Parsing error: " // trim(error_msg)
            error stop 1
        end if
    end if

    call emit_fortran(arena, root_index, output_code)

    if (index(output_code, "trait IComparable") == 0) then
        write (output_unit, '(A)') "FAIL: trait block missing from output"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    if (index(output_code, "requirement Ordered(T)") == 0) then
        write (output_unit, '(A)') "FAIL: requirement block missing from output"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    if (index(output_code, "implements IComparable(T)") == 0) then
        write (output_unit, '(A)') "FAIL: implements block missing from output"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    if (index(output_code, "implements(IComparable)") == 0) then
        write (output_unit, '(A)') "FAIL: implements(...) type attribute missing"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    if (index(output_code, "min_value{IComparable::T}") == 0) then
        write (output_unit, '(A)') "FAIL: trait-bounded generic missing from name"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

    write (output_unit, '(A)') "PASS: Parsed traits/requirements/implements"

contains

    include '../common/read_example.inc'
end program test_lfortran_traits_requirements_implements_parsing
