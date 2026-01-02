program test_issue_2018_duplicate_loop_var
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_function_with_explicit_loop_var()
    print *, 'PASS: Issue 2018 duplicate loop variable preserved'

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'


    subroutine test_function_with_explicit_loop_var()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        call read_example( &
            'examples/f90/issue_2018_function_returns_array_duplicate_var.f90', &
            source)

        arena = create_ast_arena()
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'Lexing error: ' // trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'Parsing error: ' // trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output)

        if (count_occurrences(output, 'integer :: i') /= 2) then
            write (error_unit, '(A)') &
                "FAIL: Expected 2 'integer :: i' declarations (main + function)"
            write (error_unit, '(A)') trim(output)
            error stop 1
        end if

        if (index(output, 'integer :: i') == 0) then
            write (error_unit, '(A)') 'FAIL: loop variable declaration missing'
            error stop 1
        end if
    end subroutine test_function_with_explicit_loop_var

    integer function count_occurrences(text, pattern)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        integer :: start_pos
        integer :: found_pos
        integer :: pattern_len

        pattern_len = len_trim(pattern)
        if (pattern_len == 0) then
            count_occurrences = 0
            return
        end if

        count_occurrences = 0
        start_pos = 1

        do
            if (start_pos > len(text)) exit
            found_pos = index(text(start_pos:), pattern(1:pattern_len))
            if (found_pos == 0) exit
            count_occurrences = count_occurrences + 1
            start_pos = start_pos + found_pos + pattern_len - 1
        end do
    end function count_occurrences

end program test_issue_2018_duplicate_loop_var
