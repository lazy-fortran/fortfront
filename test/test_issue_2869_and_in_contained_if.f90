program test_issue_2869_and_in_contained_if
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call verify_logical_condition_preserved()
    print *, ""
    print *, "Issue 2869 contained-IF logical-operator tests completed."

contains

    subroutine verify_logical_condition_preserved()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = &
            "program p" // new_line('a') // &
            "contains" // new_line('a') // &
            "  subroutine check(x, y)" // new_line('a') // &
            "    integer, intent(in) :: x, y" // new_line('a') // &
            "    if (x >= 0 .and. y >= 0) then" // new_line('a') // &
            "      print *, 'both'" // new_line('a') // &
            "    else" // new_line('a') // &
            "      print *, 'not both'" // new_line('a') // &
            "    end if" // new_line('a') // &
            "  end subroutine" // new_line('a') // &
            "end program" // new_line('a')

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

        print *, "Output:"
        print *, trim(output_code)

        if (index(output_code, "x >= 0 .and. y >= 0") == 0) then
            print *, "FAIL: .and. right operand dropped from contained-proc IF"
            error stop 1
        end if

        print *, "[PASS] logical .and. condition preserved in contained IF"
    end subroutine verify_logical_condition_preserved

end program test_issue_2869_and_in_contained_if
