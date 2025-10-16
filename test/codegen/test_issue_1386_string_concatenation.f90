program test_issue_1386_string_concatenation
    use frontend_core, only: emit_fortran
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_base, only: LITERAL_STRING
    use ast_factory, only: push_program, push_assignment, push_identifier, &
                           push_literal, push_binary_op
    implicit none

    type(ast_arena_t) :: arena
    integer :: lazy_id
    integer :: seg1_id, seg2_id, seg3_id
    integer :: concat_pair_idx, concat_chain_idx
    integer :: dangling_concat_idx
    integer :: assign_idx, prog_idx
    character(len=:), allocatable :: generated

    print *, '=== Issue #1386: guard concatenation without left operand ==='

    arena = create_ast_arena()

    lazy_id = push_identifier(arena, 'lazy_code')
    seg1_id = push_literal(arena, '"segment1"', LITERAL_STRING)
    seg2_id = push_literal(arena, '"segment2"', LITERAL_STRING)
    seg3_id = push_literal(arena, '"segment3"', LITERAL_STRING)

    concat_pair_idx = push_binary_op(arena, seg1_id, seg2_id, '//')
    concat_chain_idx = push_binary_op(arena, concat_pair_idx, seg3_id, '//')

    ! Reproduce issue #1386 by creating a dangling operator with an absent left operand.
    dangling_concat_idx = push_binary_op(arena, 0, concat_chain_idx, '//')

    assign_idx = push_assignment(arena, lazy_id, dangling_concat_idx)
    prog_idx = push_program(arena, 'demo', [assign_idx])

    call emit_fortran(arena, prog_idx, generated)

    if (.not. allocated(generated)) then
        print *, 'FAIL: emit_fortran returned no code'
        stop 1
    end if

    if (index(generated, 'lazy_code = //') > 0) then
        print *, 'FAIL: Generated assignment still begins with dangling concatenation'
        print *, trim(generated)
        stop 1
    end if

    if (index(generated, 'segment1') == 0) then
        print *, 'FAIL: Leading literal was lost during code generation'
        print *, trim(generated)
        stop 1
    end if

    print *, 'PASS: Missing left operand handled without invalid //'
end program test_issue_1386_string_concatenation
