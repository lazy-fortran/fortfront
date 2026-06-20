program test_issue_2812_lazy_dummy_type
    ! Issue #2812: the lazy standardizer synthesized a dummy intent-restatement
    ! declaration_node whose type_name was read from a stale type arena via the
    ! type-bound to_string. The bytes were uninitialized garbage, so AST
    ! consumers (e.g. the ffc backend) saw a nondeterministic, unresolved type.
    !
    ! The standardized AST must carry a concrete, ASCII-clean type_name for the
    ! inferred kind. The dummies a, b are inferred integer from add(5, 3).
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: lex_source, parse_tokens, create_ast_arena, &
                         ast_arena_t, token_t
    use semantic_api, only: semantic_context_t, create_semantic_context, &
                            analyze_program
    use standardizer, only: standardize_ast
    use ast_nodes_data, only: declaration_node
    implicit none

    character(len=:), allocatable :: source_code
    integer :: iter
    integer, parameter :: n_iterations = 20

    call read_example('examples/lf/issue_2812_lazy_toplevel_function.lf', &
        & source_code)

    do iter = 1, n_iterations
        call run_and_check(source_code, iter)
    end do

    print *, 'PASS: Issue #2812 lazy dummy type concrete and deterministic'

contains

    include '../common/read_example.inc'

    subroutine run_and_check(source_code, iter)
        character(len=*), intent(in) :: source_code
        integer, intent(in) :: iter
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(semantic_context_t) :: ctx
        character(len=:), allocatable :: err
        integer :: prog_index
        logical :: found_a, found_b

        call lex_source(source_code, tokens, err)
        call assert_no_error(err, 'lex')
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, err)
        call assert_no_error(err, 'parse')
        call create_semantic_context(ctx)
        call analyze_program(ctx, arena, prog_index)
        call standardize_ast(arena, prog_index)

        found_a = .false.
        found_b = .false.
        call inspect_declarations(arena, iter, found_a, found_b)

        if (.not. found_a) then
            write (error_unit, '(A,I0)') &
                'FAIL: dummy declaration for a missing at iteration ', iter
            error stop 1
        end if
        if (.not. found_b) then
            write (error_unit, '(A,I0)') &
                'FAIL: dummy declaration for b missing at iteration ', iter
            error stop 1
        end if
    end subroutine run_and_check

    subroutine inspect_declarations(arena, iter, found_a, found_b)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: iter
        logical, intent(inout) :: found_a, found_b
        integer :: i

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
            type is (declaration_node)
                call check_declaration(node, iter)
                if (trim(node%var_name) == 'a') then
                    call assert_integer_type(node%type_name, 'a', iter)
                    found_a = .true.
                else if (trim(node%var_name) == 'b') then
                    call assert_integer_type(node%type_name, 'b', iter)
                    found_b = .true.
                end if
            end select
        end do
    end subroutine inspect_declarations

    subroutine check_declaration(node, iter)
        type(declaration_node), intent(in) :: node
        integer, intent(in) :: iter

        ! Guarantee: no declaration_node may carry an unallocated type_name.
        if (.not. allocated(node%type_name)) then
            write (error_unit, '(A,A,A,I0)') &
                'FAIL: unallocated type_name for ', trim(node%var_name), &
                ' at iteration ', iter
            error stop 1
        end if
        call assert_ascii_clean(node%type_name, node%var_name, iter)
    end subroutine check_declaration

    subroutine assert_integer_type(type_name, var, iter)
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: var
        integer, intent(in) :: iter

        if (trim(type_name) /= 'integer') then
            write (error_unit, '(A,A,A,A,A,I0)') &
                'FAIL: dummy ', trim(var), ' type_name not integer (got [', &
                trim(type_name), ']) at iteration ', iter
            error stop 1
        end if
    end subroutine assert_integer_type

    subroutine assert_ascii_clean(text, var, iter)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: var
        integer, intent(in) :: iter
        integer :: i, code

        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code == 9 .or. code == 32) cycle
            if (code < 32 .or. code > 126) then
                write (error_unit, '(A,A,A,I0,A,I0)') &
                    'FAIL: non-ASCII byte in type_name of ', trim(var), &
                    ' code=', code, ' at iteration ', iter
                error stop 1
            end if
        end do
    end subroutine assert_ascii_clean

    subroutine assert_no_error(message, phase)
        character(len=:), allocatable, intent(in) :: message
        character(len=*), intent(in) :: phase

        if (.not. allocated(message)) return
        if (len_trim(message) == 0) return

        write (error_unit, '(A,A,A,A)') 'FAIL: ', phase, ' error: ', &
            trim(message)
        error stop 1
    end subroutine assert_no_error

end program test_issue_2812_lazy_dummy_type
