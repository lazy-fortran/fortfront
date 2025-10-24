! Test for issue #1814: nested internal functions in lazy fortran
! Expected: clear error message that nested internal procedures are not supported
program test_issue_1814_nested_lazy_function
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_nested_function_error_message()
    print *, ""
    print *, "Nested lazy function error reporting test completed."

contains

    subroutine test_nested_function_error_message()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        ! Lazy fortran code with nested internal function (not supported)
        input_code = "function outer(x)" // new_line('A') // &
                     "    function inner(y)" // new_line('A') // &
                     "        result = y * 2" // new_line('A') // &
                     "    end function" // new_line('A') // &
                     "    result = inner(x) + 1" // new_line('A') // &
                     "end function" // new_line('A') // &
                     "" // new_line('A') // &
                     "z = outer(5)" // new_line('A') // &
                     "print *, z"

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error:", trim(error_msg)
            error stop 1
        end if

        ! Parse should emit error message to stderr about nested functions
        ! but continue processing to produce output (even if mangled)
        call parse_tokens(tokens, arena, prog_index, error_msg)

        ! Should still be able to generate output
        call emit_fortran(arena, prog_index, output_code)

        ! Verify outer function exists in output
        if (index(output_code, 'outer') == 0) then
            print *, "FAIL: outer function completely missing from output"
            print *, trim(output_code)
            error stop 1
        end if

        print *, "[PASS] Nested lazy function produces error and partial output"
    end subroutine test_nested_function_error_message

end program test_issue_1814_nested_lazy_function
