program test_issue_2415_no_space
    use, intrinsic :: iso_fortran_env, only: output_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    character(len=:), allocatable :: source1, source2, output1, output2
    character(len=:), allocatable :: error_msg
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    integer :: root

    source1 = "program test" // new_line('a') // &
        "integer :: x" // new_line('a') // &
        "data x / 5 /" // new_line('a') // &
        "end program"

    source2 = "program test" // new_line('a') // &
        "integer :: x" // new_line('a') // &
        "data x /5/" // new_line('a') // &
        "end program"

    write (output_unit, '(A)') "Testing with spaces: data x / 5 /"
    arena = create_ast_arena()
    call lex_source(source1, tokens, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Lex error: " // trim(error_msg)
        error stop 1
    end if

    call parse_tokens(tokens, arena, root, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Parse error: " // trim(error_msg)
        error stop 1
    end if

    call emit_fortran(arena, root, output1)
    write (output_unit, '(A)') "Emitted: " // trim(output1)

    write (output_unit, '(A)') ""
    write (output_unit, '(A)') "Testing without spaces: data x /5/"
    arena = create_ast_arena()
    call lex_source(source2, tokens, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Lex error (no space): " // trim(error_msg)
        error stop 1
    end if

    call parse_tokens(tokens, arena, root, error_msg)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (output_unit, '(A)') "FAIL: Parse error (no space): "// &
            trim(error_msg)
        error stop 1
    end if

    call emit_fortran(arena, root, output2)
    write (output_unit, '(A)') "Emitted: " // trim(output2)

    write (output_unit, '(A)') "PASS: Both formats parse successfully"

end program test_issue_2415_no_space
