program test_issue_2813_value_attribute
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call verify_value_roundtrip()
    print *, ""
    print *, "Issue 2813 VALUE attribute tests completed."

contains

    include 'common/read_example.inc'

    subroutine verify_value_roundtrip()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        integer :: value_count

        call read_example('examples/f90/issue_2813_value_attribute.f90', &
                          input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error:", trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        value_count = count_occurrences(output_code, ', value')
        if (value_count < 3) then
            print *, "FAIL: VALUE attribute not preserved on all dummies"
            print *, "Expected at least 3 ', value', found", value_count
            print *, "Output:", output_code
            error stop 1
        end if
        print *, "PASS: VALUE attribute preserved (", value_count, "dummies)"

        if (index(output_code, ':: value') /= 0) then
            print *, "FAIL: VALUE treated as variable name"
            print *, "Output:", output_code
            error stop 1
        end if
        print *, "PASS: VALUE not treated as variable name"
    end subroutine verify_value_roundtrip

    integer function count_occurrences(haystack, needle) result(n)
        character(len=*), intent(in) :: haystack
        character(len=*), intent(in) :: needle
        integer :: pos, hit

        n = 0
        pos = 1
        do
            hit = index(haystack(pos:), needle)
            if (hit == 0) exit
            n = n + 1
            pos = pos + hit + len(needle) - 1
            if (pos > len(haystack)) exit
        end do
    end function count_occurrences

end program test_issue_2813_value_attribute
