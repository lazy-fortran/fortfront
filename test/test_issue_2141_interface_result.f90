program test_issue_2141_interface_result
    use, intrinsic :: iso_fortran_env, only: output_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: input_code, output_code, error_msg
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: root_index
    logical :: test_passed

    test_passed = .true.

    ! Test case from issue #2141: interface block with result() clause
    input_code = "interface" // new_line('A') // &
        "    function external_func(x) result(y)" // new_line('A') // &
        "        real, intent(in) :: x" // new_line('A') // &
        "        real :: y" // new_line('A') // &
        "    end function external_func" // new_line('A') // &
        "end interface" // new_line('A')

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

    ! Check that NO EXTERNAL declaration was added
    ! The bug was adding: real, external :: external_func
    if (index(output_code, "external :: external_func") > 0) then
        write (output_unit, '(A)') &
            "FAIL: EXTERNAL declaration should not be added for interface functions"
        test_passed = .false.
    end if

    if (index(output_code, ", external ::") > 0) then
        write (output_unit, '(A)') &
            "FAIL: No EXTERNAL declarations should appear for interface blocks"
        test_passed = .false.
    end if

    ! Check that interface block is preserved
    if (index(output_code, "interface") == 0) then
        write (output_unit, '(A)') "FAIL: interface keyword not found in output"
        test_passed = .false.
    end if

    if (index(output_code, "end interface") == 0) then
        write (output_unit, '(A)') "FAIL: end interface not found in output"
        test_passed = .false.
    end if

    ! Check that result clause is preserved
    if (index(output_code, "result(y)") == 0) then
        write (output_unit, '(A)') "FAIL: result(y) clause not found in output"
        test_passed = .false.
    end if

    ! Check that result variable declaration is preserved
    if (index(output_code, "real :: y") == 0) then
        write (output_unit, '(A)') "FAIL: result variable declaration not found"
        test_passed = .false.
    end if

    if (test_passed) then
        write (output_unit, '(A)') "PASS: Interface block with result() correctly handled"
    else
        write (output_unit, '(A)') "Generated code:"
        write (output_unit, '(A)') output_code
        error stop 1
    end if

end program test_issue_2141_interface_result
