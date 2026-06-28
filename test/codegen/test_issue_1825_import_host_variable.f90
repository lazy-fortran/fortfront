program test_issue_1825_import_host_variable
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
    integer :: pos_import, pos_decl, pos_interface
    logical :: passed
    character(len=1), parameter :: nl = new_line('A')

    input_code = "program test_import_statement" // nl // &
        "    implicit none" // nl // &
        "    integer :: outer_var" // nl // &
        "    " // nl // &
        "    outer_var = 5" // nl // &
        "    " // nl // &
        "    interface" // nl // &
        "        subroutine sub_with_import(x)" // nl // &
        "            import :: outer_var" // nl // &
        "            integer, intent(in) :: x" // nl // &
        "        end subroutine sub_with_import" // nl // &
        "    end interface" // nl // &
        "    " // nl // &
        "    print *, outer_var" // nl // &
        "end program test_import_statement"

    passed = .true.
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

    pos_decl = index(output_code, "integer :: outer_var")
    pos_interface = index(output_code, "interface")
    pos_import = index(output_code, "import :: outer_var")

    if (pos_decl == 0) then
        write (output_unit, '(A)') "FAIL: outer_var declaration missing in output"
        passed = .false.
    end if

    if (pos_interface == 0) then
        write (output_unit, '(A)') "FAIL: interface block missing in output"
        passed = .false.
    end if

    if (pos_import == 0) then
        write (output_unit, '(A)') "FAIL: import statement missing in output"
        passed = .false.
    end if

    if (pos_decl > 0 .and. pos_interface > 0) then
        if (pos_decl > pos_interface) then
            write (output_unit, '(A)') &
                "FAIL: outer_var declaration appears after interface block"
            write (output_unit, '(A)') "Generated code:"
            write (output_unit, '(A)') trim(output_code)
            passed = .false.
        end if
    end if

    if (pos_interface > 0 .and. pos_import > 0) then
        if (pos_import < pos_interface .or. pos_import > pos_interface + 200) then
            write (output_unit, '(A)') &
                "FAIL: import statement not inside interface block"
            passed = .false.
        end if
    end if

    if (passed) then
        write (output_unit, '(A)') &
            "PASS: outer_var declaration precedes interface with import"
    else
        if (pos_decl > pos_interface) then
            write (output_unit, '(A)') "Generated code:"
            write (output_unit, '(A)') trim(output_code)
        end if
        error stop 1
    end if

end program test_issue_1825_import_host_variable
