program test_ast_structure_no_json
    use lexer_api, only: lex_source
    use parser_api, only: parse_tokens
    use lexer_token_types, only: token_t
    use ast_arena_modern, only: ast_arena_t
    use ast_traversal, only: is_program_node
    implicit none

    logical :: ok
    ok = .true.

    call test_basic_ast(ok)

    if (ok) then
        print *, 'AST structure tests passed (no JSON)'
    else
        print *, 'AST structure tests FAILED'
        stop 1
    end if

contains

    subroutine test_basic_ast(ok)
        logical, intent(inout) :: ok
        character(len=*), parameter :: src = &
            'program test'//new_line('a')// &
            '  integer :: x, y'//new_line('a')// &
            '  x = 10'//new_line('a')// &
            '  y = x + 5'//new_line('a')// &
            'end program test'
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: prog_index
        character(len=:), allocatable :: err
        logical :: pass

        pass = .true.
        call lex_source(src, tokens, err)
        if (allocated(err) .and. len_trim(err) > 0) then
            print *, '  FAIL: lex error: ', err
            ok = .false.; return
        end if

        call parse_tokens(tokens, arena, prog_index, err)
        if (allocated(err) .and. len_trim(err) > 0) then
            print *, '  FAIL: parse error: ', err
            ok = .false.; return
        end if

        if (prog_index <= 0) then
            print *, '  FAIL: invalid program index'
            pass = .false.
        end if

        if (.not. is_program_node(arena, prog_index)) then
            print *, '  FAIL: root is not a program node'
            pass = .false.
        end if

        if (pass) then
            print *, '  PASS: basic AST constructed and identified'
        end if
        ok = ok .and. pass
    end subroutine test_basic_ast

end program test_ast_structure_no_json
