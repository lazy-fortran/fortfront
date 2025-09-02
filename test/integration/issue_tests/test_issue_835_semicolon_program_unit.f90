program test_issue_835_semicolon_program_unit
    ! Regression test for Issue #835: semicolons in explicit program units
    use iso_fortran_env, only: error_unit
    use frontend_core, only: lex_source, emit_fortran
    use frontend_parsing, only: parse_tokens
    use ast_core, only: ast_arena_t, create_ast_arena
    use lexer_core, only: token_t
    implicit none

    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    character(len=:), allocatable :: error_msg, code, source
    integer :: prog_index
    logical :: ok

    print *, "=== Testing Issue #835: Program unit with semicolons ==="

    ok = .true.

    source = 'program test; x = 42; print *, x; end program'

    call lex_source(source, tokens, error_msg)
    if (error_msg /= "") then
        print *, "LEXING ERROR:", error_msg
        ok = .false.
    else
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (error_msg /= "") then
            print *, "PARSING ERROR:", error_msg
            ok = .false.
        else
            call emit_fortran(arena, prog_index, code)
            if (index(code, 'x = 42') <= 0) then
                print *, 'FAIL: assignment missing in output'
                print *, trim(code)
                ok = .false.
            end if
            if (index(code, 'print *, x') <= 0) then
                print *, 'FAIL: print statement missing in output'
                print *, trim(code)
                ok = .false.
            end if
            if (index(code, "!ERROR: Unrecognized operator '") > 0) then
                print *, 'FAIL: unexpected operator error leaked into output'
                print *, trim(code)
                ok = .false.
            end if
        end if
    end if

    if (.not. ok) stop 1
end program test_issue_835_semicolon_program_unit

