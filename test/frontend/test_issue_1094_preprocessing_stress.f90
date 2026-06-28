program test_issue_1094_preprocessing_stress
    ! Regression test for Issue #1094 (preprocessing crash with many statements)
    use fortfront, only: lex_source, parse_tokens, token_t, ast_arena_t, create_ast_arena
    use ast_nodes_core, only: program_node
    implicit none

    logical :: ok
    ok = .true.

    print *, 'Testing preprocessing robustness with many statements (Issue #1094)...'

    if (.not. run_stress_case()) ok = .false.

    if (ok) then
        print *, 'PASS: preprocessing stress does not crash'
        stop 0
    else
        print *, 'FAIL: preprocessing stress case'
        stop 1
    end if

contains

    logical function run_stress_case()
        character(len=:), allocatable :: src
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: err
        integer :: prog_idx
        integer :: i

        run_stress_case = .true.

        ! Build a source with 60 sequential assignments to stress preprocessing
        src = 'sum = 0' // new_line('A')
        do i = 1, 60
            src = src // 'sum = sum + ' // trim(adjustl(itoa(i))) // new_line('A')
        end do

        call lex_source(src, tokens, err)
        if (err /= '') then
            print *, '  FAIL: lexing error:', err
            run_stress_case = .false.
            return
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_idx, err)
        if (err /= '') then
            print *, '  FAIL: parsing error:', err
            run_stress_case = .false.
            return
        end if

        if (prog_idx <= 0 .or. prog_idx > arena%size) then
            print *, '  FAIL: invalid program index:', prog_idx
            run_stress_case = .false.
            return
        end if

        select type (p => arena%entries(prog_idx)%node)
            type is (program_node)
            if (size(p%body_indices) < 61) then
                print *, '  FAIL: expected >= 61 statements, got', size(p%body_indices)
                run_stress_case = .false.
                return
            end if
        class default
            print *, '  FAIL: expected program_node type'
            run_stress_case = .false.
            return
        end select
    end function run_stress_case

    pure function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=32) :: buf
        write (buf, '(I0)') n
        s = trim(buf)
    end function itoa

end program test_issue_1094_preprocessing_stress

