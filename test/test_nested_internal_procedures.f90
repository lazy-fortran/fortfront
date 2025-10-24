! Regression tests for nested internal procedures within procedure bodies
program test_nested_internal_procedures
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_nested_contains_preserves_prefixes()
    print *, ""
    print *, "Nested internal procedure tests completed."

contains

    subroutine test_nested_contains_preserves_prefixes()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        integer :: contains_pos
        integer :: inner_pos
        integer :: outer_pos

        input_code = "program nested_demo" // new_line('A') // &
                     "contains" // new_line('A') // &
                     "function outer(x) result(res)" // new_line('A') // &
                     "integer, intent(in) :: x" // new_line('A') // &
                     "integer :: res" // new_line('A') // &
                     "res = inner(x) + 1" // new_line('A') // &
                     "contains" // new_line('A') // &
                     "integer function inner(y)" // new_line('A') // &
                     "integer, intent(in) :: y" // new_line('A') // &
                     "inner = y" // new_line('A') // &
                     "end function inner" // new_line('A') // &
                     "pure subroutine helper(z)" // new_line('A') // &
                     "integer, intent(in) :: z" // new_line('A') // &
                     "end subroutine helper" // new_line('A') // &
                     "end function outer" // new_line('A') // &
                     "end program nested_demo"

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

        outer_pos = index(output_code, 'function outer')
        contains_pos = index(output_code, 'contains')
        inner_pos = index(output_code, 'integer function inner')

        if (contains_pos == 0 .or. inner_pos <= contains_pos .or. &
            (outer_pos > 0 .and. contains_pos < outer_pos)) then
            print *, "FAIL: nested contains structure missing or misplaced"
            print *, trim(output_code)
            error stop 1
        end if
        if (outer_pos == 0) then
            print *, "FAIL: outer function header missing"
            print *, trim(output_code)
            error stop 1
        end if

        if (inner_pos == 0) then
            print *, "FAIL: inner function header missing"
            print *, trim(output_code)
            error stop 1
        end if

        if (.not. contains_without_spaces(output_code, 'puresubroutinehelper')) then
            print *, "FAIL: pure prefix missing from nested subroutine"
            print *, trim(output_code)
            error stop 1
        end if

        print *, "[PASS] Nested contains preserves structure and prefixes"
    end subroutine test_nested_contains_preserves_prefixes

    logical function contains_without_spaces(text, pattern)
        character(*), intent(in) :: text
        character(*), intent(in) :: pattern
        character(:), allocatable :: compressed

        compressed = remove_spaces(adjustl(text))
        contains_without_spaces = index(compressed, pattern) > 0
    end function contains_without_spaces

    pure function remove_spaces(value) result(clean)
        character(*), intent(in) :: value
        character(:), allocatable :: clean
        integer :: i

        clean = ''
        do i = 1, len(value)
            if (value(i:i) /= ' ' .and. value(i:i) /= new_line('A')) then
                clean = clean // value(i:i)
            end if
        end do
    end function remove_spaces

end program test_nested_internal_procedures
