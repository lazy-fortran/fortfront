program test_issue_1707_import_order
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
    integer :: pos_import, pos_decl
    logical :: passed
    character(len=1), parameter :: nl = new_line('A')

    input_code = "module reorder_import" // nl // &
        "    type data_t" // nl // &
        "        integer value" // nl // &
        "    end type" // nl // &
        "" // nl // &
        "    interface" // nl // &
        "        subroutine process_data(d)" // nl // &
        "            import data_t" // nl // &
        "            type(data_t) d" // nl // &
        "        end subroutine process_data" // nl // &
        "    end interface" // nl // &
        "end module"

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

    pos_import = index(output_code, "import data_t")
    pos_decl = index(output_code, "type(data_t) :: d")

    if (pos_import == 0) then
        write (output_unit, '(A)') "FAIL: import statement missing in output"
        passed = .false.
    end if

    if (pos_decl == 0) then
        write (output_unit, '(A)') "FAIL: dummy argument declaration missing"
        passed = .false.
    end if

    if (pos_import > 0 .and. pos_decl > 0) then
        if (pos_import > pos_decl) then
            write (output_unit, '(A)') &
                "FAIL: import appears after dummy argument declaration"
            passed = .false.
        end if
    end if

    if (passed) then
        write (output_unit, '(A)') &
            "PASS: import precedes dummy argument declaration"
    else
        write (output_unit, '(A)') "Generated code:"
        write (output_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (allocated(tokens)) deallocate (tokens)
    arena = create_ast_arena()
    passed = .true.

    input_code = "module reorder_assignment" // nl // &
        "    implicit none" // nl // &
        "contains" // nl // &
        "    subroutine process()" // nl // &
        "        integer :: important_value" // nl // &
        "        important_value = 1" // nl // &
        "    end subroutine process" // nl // &
        "end module reorder_assignment"

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

    pos_decl = index(output_code, "integer :: important_value")
    pos_import = index(output_code, "important_value = 1")

    if (pos_decl == 0) then
        write (output_unit, '(A)') &
            "FAIL: important_value declaration missing in output"
        passed = .false.
    end if

    if (pos_import == 0) then
        write (output_unit, '(A)') &
            "FAIL: important_value assignment missing in output"
        passed = .false.
    end if

    if (pos_decl > 0 .and. pos_import > 0) then
        if (pos_import < pos_decl) then
            write (output_unit, '(A)') &
                "FAIL: important_value assignment moved before declaration"
            passed = .false.
        end if
    end if

    if (passed) then
        write (output_unit, '(A)') &
            "PASS: statements prefixed with important remain after declarations"
    else
        write (output_unit, '(A)') "Generated code:"
        write (output_unit, '(A)') trim(output_code)
        error stop 1
    end if

end program test_issue_1707_import_order
