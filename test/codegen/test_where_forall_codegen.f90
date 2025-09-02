program test_where_forall_codegen
    use frontend_core, only: emit_fortran
    use ast_core, only: ast_arena_t, create_ast_arena, LITERAL_INTEGER
    use ast_factory, only: push_program, push_identifier, push_literal, push_binary_op, &
                           push_assignment
    use ast_factory, only: push_where_construct_with_elsewhere, push_forall
    implicit none

    logical :: all_passed
    all_passed = .true.

    if (.not. test_where_codegen()) all_passed = .false.
    if (.not. test_forall_codegen()) all_passed = .false.

    if (all_passed) then
        print *, 'All WHERE/FORALL codegen tests passed!'
    else
        print *, 'Some WHERE/FORALL codegen tests failed!'
    end if

contains

    logical function test_where_codegen()
        type(ast_arena_t) :: arena
        integer :: mask_idx, a_id, zero_id, one_id
        integer :: rhs_add_idx, assign_then_idx, assign_else_idx
        integer :: where_idx, prog_idx
        character(len=:), allocatable :: code

        test_where_codegen = .true.
        print *, 'Testing WHERE construct code generation...'

        arena = create_ast_arena()

        ! Build mask: a > 0
        a_id    = push_identifier(arena, 'a')
        zero_id = push_literal(arena, '0', LITERAL_INTEGER)
        mask_idx = push_binary_op(arena, a_id, zero_id, '>')

        ! THEN body: a = a + 1
        one_id = push_literal(arena, '1', LITERAL_INTEGER)
        rhs_add_idx = push_binary_op(arena, a_id, one_id, '+')
        assign_then_idx = push_assignment(arena, a_id, rhs_add_idx)

        ! ELSEWHERE body: a = 0
        assign_else_idx = push_assignment(arena, a_id, zero_id)

        ! WHERE with ELSEWHERE
        where_idx = push_where_construct_with_elsewhere(arena, mask_idx, &
                        where_body_indices=[assign_then_idx], &
                        elsewhere_body_indices=[assign_else_idx])

        prog_idx = push_program(arena, 'main', [where_idx])

        call emit_fortran(arena, prog_idx, code)

        if (.not. allocated(code)) then
            print *, '  FAIL: No code generated for WHERE'
            test_where_codegen = .false.
            return
        end if

        if (index(code, 'where (') == 0) then
            print *, '  FAIL: Missing WHERE header'
            test_where_codegen = .false.
        end if

        if (index(code, 'elsewhere') == 0) then
            print *, '  FAIL: Missing ELSEWHERE clause'
            test_where_codegen = .false.
        end if

        if (index(code, 'end where') == 0) then
            print *, '  FAIL: Missing END WHERE'
            test_where_codegen = .false.
        end if
    end function test_where_codegen

    logical function test_forall_codegen()
        type(ast_arena_t) :: arena
        integer :: i_id, n_id, one_id
        integer :: assign_idx, forall_idx, prog_idx
        character(len=:), allocatable :: code

        test_forall_codegen = .true.
        print *, 'Testing FORALL construct code generation...'

        arena = create_ast_arena()

        ! Triplet: i = 1:n (no stride)
        i_id   = push_identifier(arena, 'i')
        one_id = push_literal(arena, '1', LITERAL_INTEGER)
        n_id   = push_identifier(arena, 'n')

        ! Body: i = 1 (arbitrary simple assignment for codegen presence)
        assign_idx = push_assignment(arena, i_id, one_id)

        forall_idx = push_forall(arena, 'i', one_id, n_id, 0, 0, [assign_idx])

        prog_idx = push_program(arena, 'main', [forall_idx])

        call emit_fortran(arena, prog_idx, code)

        if (.not. allocated(code)) then
            print *, '  FAIL: No code generated for FORALL'
            test_forall_codegen = .false.
            return
        end if

        if (index(code, 'forall (') == 0) then
            print *, '  FAIL: Missing FORALL header'
            test_forall_codegen = .false.
        end if

        if (index(code, 'end forall') == 0) then
            print *, '  FAIL: Missing END FORALL'
            test_forall_codegen = .false.
        end if
    end function test_forall_codegen

end program test_where_forall_codegen
