program test_issue_1826_enum_construct
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    character(len=1), parameter :: nl = new_line('A')
    character(len=*), parameter :: enum_error = &
        "! ERROR: Unsupported Fortran feature: enum constructs are not supported"

    call test_enum_in_program()
    call test_enum_in_module()
    print *, ""
    print *, "Issue 1826 enum construct tests completed."

contains

    subroutine test_enum_in_program()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "program test_enum" // nl // &
                     "    enum, bind(c)" // nl // &
                     "        enumerator :: RED = 1" // nl // &
                     "        enumerator :: GREEN = 2" // nl // &
                     "    end enum" // nl // &
                     "    integer :: color" // nl // &
                     "    color = RED" // nl // &
                     "    print *, color" // nl // &
                     "end program test_enum"

        print *, ""
        print *, "Test: Enum construct inside program body"
        print *, "Input:"
        print *, trim(input_code)

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

        if (index(output_code, enum_error) == 0) then
            print *, "FAIL: Missing enum unsupported feature error comment"
            error stop 1
        end if

        if (index(output_code, "RED = 1") /= 0 .or. &
            index(output_code, "GREEN = 2") /= 0 .or. &
            index(output_code, "BLUE = 3") /= 0) then
            print *, "FAIL: Enumerator assignments remain in output"
            error stop 1
        end if

        print *, "[PASS] Enum construct detected in program body"
    end subroutine test_enum_in_program

    subroutine test_enum_in_module()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        input_code = "module colors" // nl // &
                     "    enum, bind(c)" // nl // &
                     "        enumerator :: RED = 1" // nl // &
                     "        enumerator :: GREEN = 2" // nl // &
                     "        enumerator :: BLUE = 3" // nl // &
                     "    end enum" // nl // &
                     "contains" // nl // &
                     "    subroutine report()" // nl // &
                     "        print *, RED" // nl // &
                     "    end subroutine report" // nl // &
                     "end module colors" // nl // &
                     "" // nl // &
                     "program demo" // nl // &
                     "    use colors" // nl // &
                     "    call report()" // nl // &
                     "end program demo"

        print *, ""
        print *, "Test: Enum construct inside module body"
        print *, "Input:"
        print *, trim(input_code)

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

        if (index(output_code, enum_error) == 0) then
            print *, "FAIL: Missing enum unsupported feature error comment"
            error stop 1
        end if

        if (index(output_code, "RED = 1") /= 0 .or. &
            index(output_code, "GREEN = 2") /= 0 .or. &
            index(output_code, "BLUE = 3") /= 0) then
            print *, "FAIL: Enumerator assignments remain in module output"
            error stop 1
        end if

        print *, "[PASS] Enum construct detected in module body"
    end subroutine test_enum_in_module

end program test_issue_1826_enum_construct
