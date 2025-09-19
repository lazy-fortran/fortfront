! Test to diagnose module contains parsing issues 
program test_module_contains_parser_issue
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_minimal_module_with_function()
    call test_module_with_multi_arg_function()
    print *, ""
    print *, "Parser diagnostic tests completed."

contains

    subroutine test_minimal_module_with_function()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        ! Minimal module with single function
        input_code = "module m" // new_line('A') // &
                     "contains" // new_line('A') // &
                     "function f(x) result(y)" // new_line('A') // &
                     "real :: x, y" // new_line('A') // &
                     "y = x * 2.0" // new_line('A') // &
                     "end function f" // new_line('A') // &
                     "end module m"

        print *, ""
        print *, "Test: Minimal module with single-argument function"
        print *, "Input:"
        print *, trim(input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error: ", trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error: ", error_msg
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        print *, "Output:"
        print *, trim(output_code)

        ! Check for completeness
        if (index(output_code, 'function f(x) result(y)') == 0) then
            print *, "FAIL: Function signature mangled"
            error stop 1
        end if

        if (index(output_code, 'real :: x, y') == 0) then
            print *, "WARNING: Variable declarations incomplete - got:"
            call print_declaration_lines(output_code, 'real')
        end if

        print *, "[PASS] Minimal module with single-argument function"
    end subroutine test_minimal_module_with_function

    subroutine test_module_with_multi_arg_function()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index

        ! Module with multi-argument function
        input_code = "module math_utils" // new_line('A') // &
                     "contains" // new_line('A') // &
                     "function add3(a, b, c) result(sum)" // new_line('A') // &
                     "integer, intent(in) :: a, b, c" // new_line('A') // &
                     "integer :: sum" // new_line('A') // &
                     "sum = a + b + c" // new_line('A') // &
                     "end function add3" // new_line('A') // &
                     "end module math_utils"

        print *, ""
        print *, "Test: Module with multi-argument function"
        print *, "Input:"
        print *, trim(input_code)

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Lexing error: ", trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "Parsing error: ", error_msg
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        print *, "Output:"
        print *, trim(output_code)

        ! Check for completeness
        if (index(output_code, 'function add3') == 0) then
            print *, "FAIL: Function name missing"
            error stop 1
        end if

        ! Check if all arguments are present in signature
        if (index(output_code, 'add3(a, b, c)') == 0) then
            print *, "WARNING: Function signature incomplete - got:"
            call print_function_signatures(output_code)
        end if

        ! Check declarations
        if (index(output_code, 'integer, intent(in) :: a, b, c') == 0) then
            print *, "WARNING: Declaration incomplete - got:"
            call print_declaration_lines(output_code, 'integer')
        end if

        print *, "[PASS] Module with multi-argument function (with warnings)"
    end subroutine test_module_with_multi_arg_function

    subroutine print_declaration_lines(code, type_str)
        character(*), intent(in) :: code, type_str
        integer :: pos, end_pos
        pos = 1
        do
            pos = index(code(pos:), type_str)
            if (pos == 0) exit
            pos = pos + pos - 1
            end_pos = index(code(pos:), new_line('A'))
            if (end_pos == 0) end_pos = len(code) - pos + 1
            print *, "    Found: ", code(pos:pos+end_pos-1)
            pos = pos + end_pos
        end do
    end subroutine print_declaration_lines

    subroutine print_function_signatures(code)
        character(*), intent(in) :: code
        integer :: pos, end_pos
        pos = 1
        do
            pos = index(code(pos:), 'function')
            if (pos == 0) exit
            pos = pos + pos - 1
            end_pos = index(code(pos:), new_line('A'))
            if (end_pos == 0) end_pos = len(code) - pos + 1
            print *, "    Found: ", code(pos:pos+end_pos-1)
            pos = pos + end_pos
        end do
    end subroutine print_function_signatures

end program test_module_contains_parser_issue
