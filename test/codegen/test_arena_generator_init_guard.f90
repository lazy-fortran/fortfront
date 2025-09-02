program test_arena_generator_init_guard
    use ast_core, only: ast_arena_t, create_ast_arena, create_literal, literal_node, LITERAL_INTEGER
    use codegen_arena_interface, only: generate_code_from_arena
    implicit none

    type(ast_arena_t) :: arena
    type(literal_node) :: lit
    character(len=:), allocatable :: code
    integer :: idx

    print *, '=== Arena generator init guard test ==='

    ! Build a minimal arena with a single literal node
    arena = create_ast_arena()
    lit = create_literal('42', LITERAL_INTEGER, 1, 1)
    call arena%push(lit, 'literal')
    idx = arena%size

    ! Intentionally do NOT call initialize_codegen() here
    code = generate_code_from_arena(arena, idx)

    if (index(code, 'Arena generator not set (call initialize_codegen())') > 0) then
        print *, 'PASS: Helpful diagnostic emitted when uninitialized'
        stop 0
    else
        print *, 'FAIL: Expected actionable diagnostic not found'
        print *, 'Got: ', trim(code)
        error stop 1
    end if

end program test_arena_generator_init_guard

