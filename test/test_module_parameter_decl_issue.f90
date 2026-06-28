! Minimal test demonstrating parameter declaration issue in module functions
! This is related to issue #926
program test_module_parameter_decl_issue
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    call test_param_declarations()
    print *, "Test completed."

contains

    subroutine test_param_declarations()
        character(:), allocatable :: input_code
        character(:), allocatable :: output_code
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        logical :: test_passed

        ! Test that parameter declarations are preserved
        input_code = "module test" // new_line('A') // &
            "contains" // new_line('A') // &
            "function calc(a, b, c) result(res)" // new_line('A') // &
            "integer, intent(in) :: a, b, c" // new_line('A') // &
            "real :: res" // new_line('A') // &
            "res = real(a + b + c)" // new_line('A') // &
            "end function calc" // new_line('A') // &
            "end module test"

        print *, ""
        print *, "Testing parameter declaration preservation in module function"
        print *, "Expected: All three parameters (a, b, c) should have declarations"
        print *, ""
        print *, "Input code:"
        print *, "----------"
        call print_lines(input_code)
        print *, ""

        arena = create_ast_arena()
        call lex_source(input_code, tokens, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "ERROR: Lexing failed: ", trim(error_msg)
            error stop 1
        end if

        call parse_tokens(tokens, arena, prog_index, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "ERROR: Parsing failed: ", error_msg
            error stop 1
        end if

        call emit_fortran(arena, prog_index, output_code)

        print *, "Output code:"
        print *, "-----------"
        call print_lines(output_code)
        print *, ""

        ! Verify the output
        test_passed = .true.

        ! Check function signature
        if (index(output_code, 'function calc(a, b, c)') == 0) then
            print *, "ERROR: Function signature incomplete"
            test_passed = .false.
        else
            print *, "✓ Function signature preserved: calc(a, b, c)"
        end if

        ! Check parameter declaration
        if (index(output_code, 'integer, intent(in) :: a, b, c') > 0) then
            print *, "✓ Parameter declaration fully preserved"
        else if (index(output_code, 'integer') > 0 .and. index(output_code, 'intent(in)') > 0) then
            print *, "⚠ Parameter declaration present but possibly incomplete"
            print *, "  Looking for: 'integer, intent(in) :: a, b, c'"
            call find_and_print_line(output_code, 'integer')
        else
            print *, "✗ Parameter declaration missing or severely incomplete"
            test_passed = .false.
        end if

        ! Check result variable declaration
        if (index(output_code, 'real :: res') > 0) then
            print *, "✓ Result variable declaration preserved"
        else
            print *, "⚠ Result variable declaration missing or altered"
            call find_and_print_line(output_code, 'real')
        end if

        ! Check function body
        if (index(output_code, 'res = ') > 0) then
            print *, "✓ Function body preserved"
        else
            print *, "✗ Function body missing"
            test_passed = .false.
        end if

        print *, ""
        if (test_passed) then
            print *, "RESULT: Basic functionality works, but parameter declarations need attention"
        else
            print *, "RESULT: Critical issues found"
        end if

    end subroutine test_param_declarations

    subroutine print_lines(text)
        character(*), intent(in) :: text
        integer :: i, start, end_pos

        start = 1
        do i = 1, len(text)
            if (text(i:i) == new_line('A')) then
                end_pos = i - 1
                if (end_pos >= start) then
                    print *, text(start:end_pos)
                else
                    print *, "" ! Empty line
                end if
                start = i + 1
            end if
        end do
        ! Print last line if no newline at end
        if (start <= len(text)) then
            print *, text(start:)
        end if
    end subroutine print_lines

    subroutine find_and_print_line(text, search_str)
        character(*), intent(in) :: text, search_str
        integer :: pos, line_start, line_end

        pos = index(text, search_str)
        if (pos > 0) then
            ! Find start of line
            line_start = pos
            do while (line_start > 1)
                if (text(line_start - 1:line_start - 1) == new_line('A')) exit
                line_start = line_start - 1
            end do

            ! Find end of line
            line_end = pos + len(search_str) - 1
            do while (line_end < len(text))
                if (text(line_end + 1:line_end + 1) == new_line('A')) exit
                line_end = line_end + 1
            end do

            print *, "  Found: '", text(line_start:line_end), "'"
        else
            print *, "  Not found: '", search_str, "'"
        end if
    end subroutine find_and_print_line

end program test_module_parameter_decl_issue
