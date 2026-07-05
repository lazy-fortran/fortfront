program test_issue_2857_generic_interface_specifics
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: lex_source, parse_tokens, ast_arena_t, &
        create_ast_arena, token_t, find_nodes_by_type
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: lex_error
    character(len=1024) :: parse_error
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    integer :: prog_index
    integer, allocatable :: subroutine_defs(:)
    integer, allocatable :: dummy_decls(:)

    source_code = &
        'interface gen' // new_line('a') // &
        '  subroutine suba(a)' // new_line('a') // &
        '    real, allocatable :: a(:)' // new_line('a') // &
        '  end subroutine' // new_line('a') // &
        '  subroutine subp(p)' // new_line('a') // &
        '    real, pointer, intent(in) :: p(:)' // new_line('a') // &
        '  end subroutine' // new_line('a') // &
        'end interface' // new_line('a')

    call lex_source(source_code, tokens, lex_error)
    if (allocated(lex_error) .and. len_trim(lex_error) > 0) then
        write (error_unit, '(A)') 'FAIL: lex error: ' // trim(lex_error)
        error stop 1
    end if

    arena = create_ast_arena()
    call parse_tokens(tokens, arena, prog_index, parse_error)
    if (len_trim(parse_error) > 0) then
        write (error_unit, '(A)') 'FAIL: parse error: ' // trim(parse_error)
        error stop 1
    end if

    subroutine_defs = find_nodes_by_type(arena, 'subroutine_def')
    if (size(subroutine_defs) /= 2) then
        write (error_unit, '(A,I0)') &
            'FAIL: expected 2 subroutine_def nodes for interface specifics, got ', &
            size(subroutine_defs)
        error stop 1
    end if

    dummy_decls = find_nodes_by_type(arena, 'parameter_declaration')
    if (size(dummy_decls) /= 2) then
        write (error_unit, '(A,I0)') &
            'FAIL: expected 2 dummy parameter declarations, got ', size(dummy_decls)
        error stop 1
    end if

    print '(A)', &
        'PASS: Issue #2857 - generic interface specifics exposed as subroutine_def'
end program test_issue_2857_generic_interface_specifics
