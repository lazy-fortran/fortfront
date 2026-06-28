program test_issue_1566_interface_block
    use, intrinsic :: iso_fortran_env, only: output_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: input_code, output_code, error_msg
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: root_index, pos1, pos2, pos3
    logical :: test_passed

    test_passed = .true.

    input_code = "program test_interface" // new_line('A') // &
        "    implicit none" // new_line('A') // &
        new_line('A') // &
        "    interface" // new_line('A') // &
        "        subroutine external_sub(x)" // new_line('A') // &
        "            integer, intent(in) :: x" // new_line('A') // &
        "        end subroutine external_sub" // new_line('A') // &
        "    end interface" // new_line('A') // &
        new_line('A') // &
        "    call external_sub(42)" // new_line('A') // &
        "end program test_interface" // new_line('A')

    arena = create_ast_arena()
    call lex_source(input_code, tokens, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Lexing error: " // trim(error_msg)
        error stop 1
    end if

    call parse_tokens(tokens, arena, root_index, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Parsing error: " // trim(error_msg)
        error stop 1
    end if

    call emit_fortran(arena, root_index, output_code)

    pos1 = index(output_code, "interface")
    pos2 = index(output_code, "call external_sub")
    pos3 = index(output_code, "end interface")

    if (pos1 == 0) then
        write (output_unit, '(A)') "FAIL: interface keyword not found in output"
        test_passed = .false.
    end if

    if (pos3 == 0) then
        write (output_unit, '(A)') "FAIL: end interface not found in output"
        test_passed = .false.
    end if

    if (pos2 == 0) then
        write (output_unit, '(A)') "FAIL: call statement not found in output"
        test_passed = .false.
    end if

    if (pos1 > 0 .and. pos2 > 0 .and. pos3 > 0) then
        if (pos1 > pos2) then
            write (output_unit, '(A)') &
                "FAIL: interface block incorrectly placed after call statement"
            test_passed = .false.
        end if

        if (pos3 < pos1) then
            write (output_unit, '(A)') &
                "FAIL: end interface appears before interface"
            test_passed = .false.
        end if

        if (pos3 > pos2) then
            write (output_unit, '(A)') &
                "FAIL: end interface incorrectly placed after call statement"
            test_passed = .false.
        end if
    end if

    if (index(output_code, "subroutine external_sub(x)" // new_line('A') // &
        "    integer, intent(in) :: x" // new_line('A') // &
        "end subroutine external_sub" // new_line('A') // &
        "    call external_sub") > 0) then
        write (output_unit, '(A)') &
            "FAIL: interface block structure stripped and placed incorrectly"
        test_passed = .false.
    end if

    if (test_passed) then
        write (output_unit, '(A)') "PASS: Interface block correctly preserved"
    else
        write (output_unit, '(A)') "Generated code:"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

end program test_issue_1566_interface_block
