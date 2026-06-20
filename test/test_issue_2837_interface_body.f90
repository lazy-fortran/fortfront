program test_issue_2837_interface_body
    use, intrinsic :: iso_fortran_env, only: output_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    call check_example('examples/f90/issue_2837_interface_semicolon.f90', &
                       'procedure get_scalar_i', all_passed)
    call check_example('examples/f90/issue_2837_interface_type_return.f90', &
                       'type(point_t) function make_point', all_passed)

    if (all_passed) then
        write (output_unit, '(A)') &
            "PASS: Issue #2837 interface body parses semicolons and type() return"
    else
        error stop 1
    end if

contains

    subroutine check_example(path, expected_fragment, passed)
        character(len=*), intent(in) :: path, expected_fragment
        logical, intent(inout) :: passed

        character(len=:), allocatable :: source, output_code, error_msg
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: root_index

        call read_example(path, source)

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (output_unit, '(A)') "FAIL: Lexing error in "//path// &
                ": "//trim(error_msg)
            passed = .false.
            return
        end if

        call parse_tokens(tokens, arena, root_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (output_unit, '(A)') "FAIL: Parsing error in "//path// &
                ": "//trim(error_msg)
            passed = .false.
            return
        end if

        call emit_fortran(arena, root_index, output_code)

        if (index(output_code, "! COMPILATION FAILED") /= 0) then
            write (output_unit, '(A)') "FAIL: compilation failed for "//path
            write (output_unit, '(A)') output_code
            passed = .false.
            return
        end if

        if (index(output_code, expected_fragment) == 0) then
            write (output_unit, '(A)') "FAIL: missing '"//expected_fragment// &
                "' in output for "//path
            write (output_unit, '(A)') output_code
            passed = .false.
        end if
    end subroutine check_example

    include 'common/read_example.inc'
end program test_issue_2837_interface_body
