program test_select_case_codegen
    use frontend_core, only: emit_fortran
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_base, only: LITERAL_INTEGER, LITERAL_STRING
    use ast_factory, only: push_program, push_identifier, push_literal
    use ast_factory, only: push_select_case, push_select_case_with_default, &
        push_case_block, push_case_default
    use ast_factory, only: push_print_statement
    implicit none

    character(len=:), allocatable :: src
    character(len=:), allocatable :: out
    character(len=:), allocatable :: err
    logical :: ok

    print *, "=== Select Case Codegen Tests ==="

    call test_basic_select_case()
    call test_select_case_with_default()
    call test_select_case_with_empty_default()

contains

    subroutine require(cond, message)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: message
        if (.not. cond) then
            print *, 'FAIL: ', trim(message)
            stop 1
        end if
    end subroutine require

    subroutine ensure_no_error()
        if (allocated(err)) then
            call require(len_trim(err) == 0, 'Unexpected error: ' // trim(err))
        end if
    end subroutine ensure_no_error

    subroutine test_basic_select_case()
        print *, 'Testing basic select-case without default...'
        block
            type(ast_arena_t) :: arena
            integer :: x_id, one_lit, two_lit, three_lit
            integer :: print_one, print_two_three
            integer :: case1_idx, case2_idx, select_idx, prog_idx

            character(len=:), allocatable :: code

            arena = create_ast_arena()

            ! Selector and values
            x_id = push_identifier(arena, 'x')
            one_lit = push_literal(arena, '1', LITERAL_INTEGER)
            two_lit = push_literal(arena, '2', LITERAL_INTEGER)
            three_lit = push_literal(arena, '3', LITERAL_INTEGER)

            ! Bodies: print statements
            print_one = push_print_statement(arena, '*', &
                [push_literal(arena, '"one"', LITERAL_STRING)])
            print_two_three = push_print_statement(arena, '*', &
                [push_literal(arena, '"two or three"', LITERAL_STRING)])

            ! Cases
            case1_idx = push_case_block(arena, [one_lit], [print_one])
            case2_idx = push_case_block(arena, [two_lit, three_lit], [print_two_three])

            ! Select without default
            select_idx = push_select_case(arena, x_id, [case1_idx, case2_idx])

            prog_idx = push_program(arena, 'main', [select_idx])

            call emit_fortran(arena, prog_idx, code)

            call require(allocated(code), 'No code generated')
            out = code
        end block

        call require(index(out, 'select case (x)') > 0, 'Missing select case header')
        call require(index(out, 'case (1)') > 0, 'Missing case (1)')
        call require(index(out, 'case (2, 3)') > 0 .or. &
            index(out, 'case (2,3)') > 0, 'Missing case (2,3)')
        call require(index(out, 'print *, "one"') > 0 .or. &
            index(out, 'print*, "one"') > 0, 'Missing first case body print')
        call require(index(out, 'print *, "two or three"') > 0 .or. &
            index(out, 'print*, "two or three"') > 0, &
            'Missing second case body print')
        call require(index(out, 'end select') > 0, 'Missing end select')
    end subroutine test_basic_select_case

    subroutine test_select_case_with_default()
        print *, 'Testing select-case with default...'
        block
            type(ast_arena_t) :: arena
            integer :: x_id, one_lit
            integer :: print_one, print_other
            integer :: case1_idx, default_idx, select_idx, prog_idx
            character(len=:), allocatable :: code

            arena = create_ast_arena()

            x_id = push_identifier(arena, 'x')
            one_lit = push_literal(arena, '1', LITERAL_INTEGER)

            print_one = push_print_statement(arena, '*', [ &
                push_literal(arena, '"one"', LITERAL_STRING)])
            print_other = push_print_statement(arena, '*', [ &
                push_literal(arena, '"other"', LITERAL_STRING)])

            case1_idx = push_case_block(arena, [one_lit], [print_one])
            default_idx = push_case_default(arena, [print_other])

            select_idx = push_select_case_with_default(arena, x_id, [case1_idx], &
                default_idx)
            prog_idx = push_program(arena, 'main', [select_idx])

            call emit_fortran(arena, prog_idx, code)
            call require(allocated(code), 'No code generated (default)')
            out = code
        end block

        call require(index(out, 'select case (x)') > 0, &
            'Missing select case header (default)')
        call require(index(out, 'case (1)') > 0, 'Missing case (1) (default)')
        call require(index(out, 'case default') > 0, 'Missing case default')
        call require(index(out, 'print *, "other"') > 0 .or. &
            index(out, 'print*, "other"') > 0, 'Missing default case body print')
        call require(index(out, 'end select') > 0, 'Missing end select (default)')
    end subroutine test_select_case_with_default

    subroutine test_select_case_with_empty_default()
        print *, 'Testing select-case with empty default...'
        block
            type(ast_arena_t) :: arena
            integer :: x_id
            integer :: select_idx, prog_idx, default_idx
            integer, allocatable :: empty_cases(:)
            integer, allocatable :: empty_body(:)
            character(len=:), allocatable :: code

            arena = create_ast_arena()

            x_id = push_identifier(arena, 'x')

            allocate (empty_cases(0))
            allocate (empty_body(0))

            default_idx = push_case_default(arena, empty_body)
            select_idx = push_select_case_with_default(arena, x_id, empty_cases, &
                default_idx)

            prog_idx = push_program(arena, 'main', [select_idx])

            call emit_fortran(arena, prog_idx, code)
            call require(allocated(code), 'No code generated (empty default)')
            out = code
        end block

        call require(index(out, 'select case (x)') > 0, &
            'Missing header (empty default)')
        call require(index(out, 'case default') > 0, 'Missing case default (empty)')
        call require(index(out, 'end select') > 0, 'Missing end select (empty default)')
    end subroutine test_select_case_with_empty_default

end program test_select_case_codegen
