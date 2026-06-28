program test_issue_1392_output_unit_use
    use frontend_core, only: emit_fortran
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_base, only: LITERAL_STRING
    use ast_factory, only: push_program, push_literal, push_write_statement, &
        push_use_statement
    implicit none

    print *, "=== Issue #1392: ensure iso output unit use is retained ==="

    call test_inserts_use_when_missing()
    call test_augments_existing_only_clause()

contains

    subroutine require(cond, message)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: message
        if (.not. cond) then
            print *, 'FAIL: ', trim(message)
            stop 1
        end if
    end subroutine require

    subroutine ensure_contains(text, pattern, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: message
        call require(index(text, pattern) > 0, message)
    end subroutine ensure_contains

    subroutine test_inserts_use_when_missing()
        type(ast_arena_t) :: arena
        integer :: write1
        integer :: write2
        integer :: lit1
        integer :: lit2
        integer :: prog_idx
        character(len=:), allocatable :: code
        integer :: pos
        integer :: len_iso
        character(len=*), parameter :: iso_line = &
            'use, intrinsic :: iso_fortran_env, only: output_unit'

        arena = create_ast_arena()

        lit1 = push_literal(arena, '"HELLO: start"', LITERAL_STRING)
        lit2 = push_literal(arena, '"HELLO: ok"', LITERAL_STRING)

        write1 = push_write_statement(arena, 'output_unit', [lit1], '(A)')
        write2 = push_write_statement(arena, 'output_unit', [lit2], '(A)')

        prog_idx = push_program(arena, 'hello', [write1, write2])

        call emit_fortran(arena, prog_idx, code)
        call require(allocated(code), 'Code generation failed for hello program')

        call ensure_contains(code, 'write(output_unit', &
            'Missing write to output_unit')
        call ensure_contains(code, '"HELLO: start"', &
            'Missing first write literal')
        call ensure_contains(code, '"HELLO: ok"', &
            'Missing second write literal')
        call ensure_contains(code, iso_line, &
            'Missing iso_fortran_env use for output_unit')

        pos = index(code, iso_line)
        len_iso = len(iso_line)
        if (pos > 0) then
            if (pos + len_iso <= len(code)) then
                call require(index(code(pos + len_iso:), iso_line) == 0, &
                    'Duplicate iso_fortran_env use inserted')
            end if
        end if
    end subroutine test_inserts_use_when_missing

    subroutine test_augments_existing_only_clause()
        type(ast_arena_t) :: arena
        integer :: write_idx
        integer :: lit_idx
        integer :: use_idx
        integer :: prog_idx
        character(len=:), allocatable :: code
        character(len=*), parameter :: iso_extended = &
            'use iso_fortran_env, only: error_unit, output_unit'

        arena = create_ast_arena()

        use_idx = push_use_statement(arena, 'iso_fortran_env', &
            only_list=[character(len=11) :: 'error_unit'], &
            has_only=.true.)

        lit_idx = push_literal(arena, '"done"', LITERAL_STRING)
        write_idx = push_write_statement(arena, 'output_unit', [lit_idx], '(A)')

        prog_idx = push_program(arena, 'hello', [use_idx, write_idx])

        call emit_fortran(arena, prog_idx, code)
        call require(allocated(code), 'Code generation failed for program with use')

        call ensure_contains(code, 'write(output_unit', &
            'Missing write to output_unit in augmented scenario')
        call ensure_contains(code, iso_extended, &
            'Missing appended output_unit in existing use statement')
        block
            integer :: pos
            integer :: len_token
            character(len=*), parameter :: token = 'use iso_fortran_env'

            pos = index(code, token)
            call require(pos > 0, 'iso_fortran_env use statement missing')
            len_token = len(token)
            if (pos > 0 .and. pos + len_token <= len(code)) then
                call require(index(code(pos + len_token:), token) == 0, &
                    'Duplicate iso_fortran_env use statements detected')
            end if
        end block
        call ensure_contains(code, 'output_unit', &
            'Missing output_unit reference in generated code')
    end subroutine test_augments_existing_only_clause

end program test_issue_1392_output_unit_use
