program test_issue_1608_class_keyword
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_class_in_subroutine_parameter()
    call test_class_in_program_body()
    print *, ""
    print *, "All CLASS keyword tests passed!"

contains

    subroutine test_class_in_subroutine_parameter()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "subroutine create_class(self, n)" // new_line('A') // &
                     "   class(atype), intent(inout) :: self" // new_line('A') // &
                     "   integer, intent(in) :: n" // new_line('A') // &
                     "end subroutine"

        print *, "=== Test 1: CLASS in subroutine parameter ==="
        print *, "Input:"
        print *, input_code

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        print *, "Output:"
        print *, output_code

        if (index(output_code, "class(atype)") > 0) then
            print *, "PASS: CLASS keyword preserved"
        else
            print *, "FAIL: CLASS keyword not found in output"
            print *, "Expected: class(atype)"
            error stop 1
        end if
    end subroutine test_class_in_subroutine_parameter

    subroutine test_class_in_program_body()
        character(:), allocatable :: input_code, output_code, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "program test" // new_line('A') // &
                     "   implicit none" // new_line('A') // &
                     "   class(mytype) :: obj" // new_line('A') // &
                     "   type(mytype2) :: obj2" // new_line('A') // &
                     "end program"

        print *, ""
        print *, "=== Test 2: CLASS in program body declaration ==="
        print *, "Input:"
        print *, input_code

        call lex_source(input_code, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Lexer error:", trim(error_msg)
            error stop 1
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "FAIL: Parser error:", trim(error_msg)
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        print *, "Output:"
        print *, output_code

        if (index(output_code, "class(mytype)") > 0) then
            print *, "PASS: CLASS keyword preserved in body declaration"
        else
            print *, "FAIL: CLASS keyword not found in output"
            error stop 1
        end if
    end subroutine test_class_in_program_body

end program test_issue_1608_class_keyword
