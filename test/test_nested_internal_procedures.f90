program test_nested_internal_procedures
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_nested_contains_preserves_prefixes()
    print *, ''
    print *, 'Nested internal procedure tests completed.'

contains

    include 'common/read_example.inc'


    subroutine test_nested_contains_preserves_prefixes()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        integer :: contains_pos
        integer :: inner_pos
        integer :: outer_pos

        call read_example('examples/lf/issue_nested_internal_procedures.lf', &
                          input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'Lexing error: ' // trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'Parsing error: ' // trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        outer_pos = index(output_code, 'function outer')
        contains_pos = index(output_code(outer_pos:), 'contains')
        inner_pos = index(output_code, 'integer function inner')

        if (contains_pos > 0) then
            contains_pos = contains_pos + outer_pos - 1
        end if

        if (contains_pos == 0 .or. inner_pos <= contains_pos .or. &
            contains_pos <= outer_pos) then
            write (error_unit, '(A)') 'FAIL: nested contains structure missing'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        if (outer_pos == 0) then
            write (error_unit, '(A)') 'FAIL: outer function header missing'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        if (inner_pos == 0) then
            write (error_unit, '(A)') 'FAIL: inner function header missing'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        if (.not. contains_without_spaces(output_code, 'puresubroutinehelper')) &
            then
            write (error_unit, '(A)') &
                'FAIL: pure prefix missing from nested subroutine'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        print *, '[PASS] Nested contains preserves structure and prefixes'
    end subroutine test_nested_contains_preserves_prefixes

    logical function contains_without_spaces(text, pattern)
        character(*), intent(in) :: text
        character(*), intent(in) :: pattern
        character(len=:), allocatable :: compressed

        compressed = remove_spaces(adjustl(text))
        contains_without_spaces = index(compressed, pattern) > 0
    end function contains_without_spaces

    pure function remove_spaces(value) result(clean)
        character(*), intent(in) :: value
        character(len=:), allocatable :: clean
        integer :: i

        clean = ''
        do i = 1, len(value)
            if (value(i:i) /= ' ' .and. value(i:i) /= new_line('A')) then
                clean = clean // value(i:i)
            end if
        end do
    end function remove_spaces

end program test_nested_internal_procedures
