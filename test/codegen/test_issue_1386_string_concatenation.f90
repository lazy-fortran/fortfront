program test_issue_1386_string_concatenation
    use frontend_core, only: emit_fortran
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_base, only: LITERAL_STRING
    use ast_factory, only: push_program, push_assignment, push_identifier, &
        push_literal, push_binary_op
    implicit none

    type(ast_arena_t) :: arena
    integer :: lazy_id
    integer :: lazy_plus_id, lazy_plus_right_id
    integer :: seg1_id, seg2_id, seg3_id
    integer :: concat_pair_idx, concat_chain_idx
    integer :: dangling_concat_idx
    integer :: dangling_plus_idx, dangling_plus_right_idx
    integer :: assign_idx, assign_plus_idx, assign_plus_right_idx
    integer :: prog_idx
    character(len=:), allocatable :: generated

    print *, '=== Issue #1386: guard concatenation without left operand ==='

    arena = create_ast_arena()

    lazy_id = push_identifier(arena, 'lazy_code')
    seg1_id = push_literal(arena, '"segment1"', LITERAL_STRING)
    seg2_id = push_literal(arena, '"segment2"', LITERAL_STRING)
    seg3_id = push_literal(arena, '"segment3"', LITERAL_STRING)
    lazy_plus_id = push_identifier(arena, 'lazy_plus')
    lazy_plus_right_id = push_identifier(arena, 'lazy_plus_right')

    concat_pair_idx = push_binary_op(arena, seg1_id, seg2_id, '//')
    concat_chain_idx = push_binary_op(arena, concat_pair_idx, seg3_id, '//')

    ! Reproduce issue #1386 by creating a dangling operator with an absent left operand.
    dangling_concat_idx = push_binary_op(arena, 0, concat_chain_idx, '//')
    dangling_plus_idx = push_binary_op(arena, 0, seg3_id, '+')
    dangling_plus_right_idx = push_binary_op(arena, seg2_id, 0, '+')

    assign_idx = push_assignment(arena, lazy_id, dangling_concat_idx)
    assign_plus_idx = push_assignment(arena, lazy_plus_id, dangling_plus_idx)
    assign_plus_right_idx = push_assignment(arena, lazy_plus_right_id, &
        dangling_plus_right_idx)
    prog_idx = push_program(arena, 'demo', [assign_idx, assign_plus_idx, &
        assign_plus_right_idx])

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

    if (index(generated, 'lazy_plus = +') > 0) then
        print *, 'FAIL: Plus-based concatenation retained leading operator'
        print *, trim(generated)
        stop 1
    end if

    if (index(generated, 'lazy_plus_right = ') > 0) then
        if (index(generated, 'lazy_plus_right = "segment2"') == 0) then
            print *, 'FAIL: Missing right operand was not reduced to left literal'
            print *, trim(generated)
            stop 1
        end if
    else
        print *, 'FAIL: Expected lazy_plus_right assignment missing'
        print *, trim(generated)
        stop 1
    end if

    print *, 'PASS: Missing left operand handled without invalid //'
end program test_issue_1386_string_concatenation
