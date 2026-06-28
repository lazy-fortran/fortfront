program test_semantic_integration
    use fortfront, only: lex_source, parse_tokens, analyze_semantics, &
        semantic_context_t, create_semantic_context, &
        emit_fortran, token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    implicit none

    type(semantic_context_t) :: context
    type(ast_arena_t) :: arena
    type(token_t), allocatable :: tokens(:)
    character(len=:), allocatable :: error_msg, code
    integer :: prog_index
    logical :: ok

    ok = .true.

    print *, "=== Semantic Integration Test ==="

    arena = create_ast_arena()
    call create_semantic_context(context)

    call lex_source('x = 42', tokens, error_msg)
    if (error_msg /= "") then
        print *, "FAIL: lex_source error: ", error_msg
        ok = .false.
    end if

    call parse_tokens(tokens, arena, prog_index, error_msg)
    if (error_msg /= "") then
        print *, "FAIL: parse_tokens error: ", error_msg
        ok = .false.
    end if

    call analyze_semantics(arena, prog_index)
    call emit_fortran(arena, prog_index, code)

    if (.not. allocated(code)) then
        print *, "FAIL: no code emitted after semantic analysis"
        ok = .false.
    else if (index(code, 'integer') == 0) then
        print *, "FAIL: emitted code lacks inferred 'integer' declaration"
        print *, "code: ", code
        ok = .false.
    else
        print *, "PASS: semantic pipeline inferred type for 'x = 42'"
    end if

    if (.not. ok) then
        print *, "=== FAILED ==="
        error stop 1
    end if

    print *, "=== PASSED ==="
end program test_semantic_integration
