program test_issue_2966_silent_statement_drops
    ! Regression test for the family of silent source-drop defects
    ! fortfront #2966, #2967, #2972, #2974 and #2977.
    !
    ! Every one of them made the frontend emit a *shorter* AST than the
    ! source: a construct, a declaration or a whole procedure vanished
    ! without any diagnostic, so no rejection test and no pass count could
    ! catch it. All five share one mechanism: a token-span or dispatch
    ! scanner comparing a keyword against its raw source spelling, or
    ! mis-counting the nesting level of a construct terminator.
    !
    ! The oracle here is the source itself: every input below is accepted
    ! by "gfortran -fsyntax-only", and each check asserts that a construct
    ! written in the source is present in the arena.
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_data, only: declaration_node
    use ast_nodes_conditional, only: if_node
    use ast_nodes_io, only: print_statement_node
    use ast_nodes_procedure, only: subroutine_def_node
    implicit none

    integer :: failures

    failures = 0

    call check_2966_uppercase_derived_type(failures)
    call check_2967_named_if_construct(failures)
    call check_2972_statement_after_nested_do(failures)
    call check_2974_multi_entity_declaration(failures)
    call check_2977_external_statement(failures)

    if (failures > 0) then
        print *, 'FAIL: ', failures, ' silent statement-drop regressions'
        error stop 1
    end if
    print *, 'PASS: no silent statement drops'

contains

    subroutine parse_source(src, arena)
        character(len=*), intent(in) :: src
        type(ast_arena_t), intent(out) :: arena
        character(:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        integer :: prog_index

        arena = create_ast_arena()
        call lex_source(src, tokens, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'Lex error: ', trim(error_msg)
                error stop 1
            end if
        end if
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'Parse error: ', trim(error_msg)
                error stop 1
            end if
        end if
    end subroutine parse_source

    integer function count_declaration(arena, name) result(hits)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        integer :: i, j

        hits = 0
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (n => arena%entries(i)%node)
            type is (declaration_node)
                if (allocated(n%var_name)) then
                    if (trim(n%var_name) == name) hits = hits + 1
                end if
                if (allocated(n%var_names)) then
                    do j = 1, size(n%var_names)
                        if (trim(n%var_names(j)) == name) hits = hits + 1
                    end do
                end if
            end select
        end do
    end function count_declaration

    subroutine expect(condition, label, failures)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label
        integer, intent(inout) :: failures

        if (.not. condition) then
            print *, 'FAIL: ', label
            failures = failures + 1
        end if
    end subroutine expect

    ! #2966: an upper-case component declaration and an upper-case END TYPE
    ! made the derived-type body swallow the rest of the module.
    subroutine check_2966_uppercase_derived_type(failures)
        integer, intent(inout) :: failures
        type(ast_arena_t) :: arena
        character(:), allocatable :: src
        logical :: found_sub
        integer :: i

        src = 'module test'//new_line('a')// &
            '    type vertex'//new_line('a')// &
            '        INTEGER :: k'//new_line('a')// &
            '    END TYPE'//new_line('a')// &
            'contains'//new_line('a')// &
            '    subroutine s1()'//new_line('a')// &
            '        integer :: i'//new_line('a')// &
            '    end subroutine'//new_line('a')// &
            'end module test'
        call parse_source(src, arena)

        found_sub = .false.
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (n => arena%entries(i)%node)
            type is (subroutine_def_node)
                if (allocated(n%name)) then
                    if (trim(n%name) == 's1') found_sub = .true.
                end if
            end select
        end do

        call expect(found_sub, '#2966: contained subroutine s1 dropped', failures)
        call expect(count_declaration(arena, 'k') >= 1, &
            '#2966: upper-case component k dropped', failures)
    end subroutine check_2966_uppercase_derived_type

    ! #2967: a named IF construct inside a contained procedure was dropped
    ! whole, because the construct name hid the construct keyword from the
    ! span scanner and from the statement dispatcher.
    subroutine check_2967_named_if_construct(failures)
        integer, intent(inout) :: failures
        type(ast_arena_t) :: arena
        character(:), allocatable :: src
        logical :: found_if
        integer :: i

        src = 'module m'//new_line('a')// &
            '    implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            '    subroutine s1()'//new_line('a')// &
            '        integer :: i'//new_line('a')// &
            '        i = 0'//new_line('a')// &
            '        check: if (i == 0) then'//new_line('a')// &
            '            i = 1'//new_line('a')// &
            '        end if check'//new_line('a')// &
            '    end subroutine s1'//new_line('a')// &
            'end module m'
        call parse_source(src, arena)

        found_if = .false.
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (n => arena%entries(i)%node)
            type is (if_node)
                if (allocated(n%then_body_indices)) then
                    if (size(n%then_body_indices) >= 1) found_if = .true.
                end if
            end select
        end do

        call expect(found_if, '#2967: named IF construct dropped', failures)
    end subroutine check_2967_named_if_construct

    ! #2972: the second token of "end do" was counted as opening a fresh DO,
    ! so the span of a nested nest never closed and every statement after it
    ! was absorbed into the loop's token span.
    subroutine check_2972_statement_after_nested_do(failures)
        integer, intent(inout) :: failures
        type(ast_arena_t) :: arena
        character(:), allocatable :: src
        logical :: found_print
        integer :: i

        src = 'module m'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            '   subroutine work()'//new_line('a')// &
            '      integer :: i, j'//new_line('a')// &
            '      do j = 1, 2'//new_line('a')// &
            '         do i = 1, 3'//new_line('a')// &
            '         end do'//new_line('a')// &
            '      end do'//new_line('a')// &
            "      print *, 'a'"//new_line('a')// &
            '   end subroutine work'//new_line('a')// &
            'end module m'
        call parse_source(src, arena)

        found_print = .false.
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (n => arena%entries(i)%node)
            type is (print_statement_node)
                found_print = .true.
            end select
        end do

        call expect(found_print, &
            '#2972: statement after nested DO nest dropped', failures)
    end subroutine check_2972_statement_after_nested_do

    ! #2974: an upper-case declaration truncated its entity list, because the
    ! attribute-keyword test compared the raw source spelling.
    subroutine check_2974_multi_entity_declaration(failures)
        integer, intent(inout) :: failures
        type(ast_arena_t) :: arena
        character(:), allocatable :: src

        src = 'SUBROUTINE s (n, a)'//new_line('a')// &
            'INTEGER n'//new_line('a')// &
            'DOUBLE PRECISION a(n+1), res'//new_line('a')// &
            'res = 2.0d0'//new_line('a')// &
            'print *, res'//new_line('a')// &
            'END'
        call parse_source(src, arena)

        call expect(count_declaration(arena, 'a') >= 1, &
            '#2974: declaration of a dropped', failures)
        call expect(count_declaration(arena, 'res') >= 1, &
            '#2974: trailing entity res dropped from entity list', failures)
    end subroutine check_2974_multi_entity_declaration

    ! #2977: EXTERNAL was absent from the statement dispatch table used for
    ! procedure bodies, so the statement produced no node at all.
    subroutine check_2977_external_statement(failures)
        integer, intent(inout) :: failures
        type(ast_arena_t) :: arena
        character(:), allocatable :: src
        logical :: found_external
        integer :: i, j

        src = 'module m_ext_min'//new_line('a')// &
            '    implicit none'//new_line('a')// &
            'contains'//new_line('a')// &
            '    subroutine s()'//new_line('a')// &
            '        external :: foo'//new_line('a')// &
            '        call foo(1)'//new_line('a')// &
            '    end subroutine s'//new_line('a')// &
            'end module m_ext_min'
        call parse_source(src, arena)

        found_external = .false.
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (n => arena%entries(i)%node)
            type is (declaration_node)
                if (.not. n%is_external) cycle
                if (allocated(n%var_name)) then
                    if (trim(n%var_name) == 'foo') found_external = .true.
                end if
                if (allocated(n%var_names)) then
                    do j = 1, size(n%var_names)
                        if (trim(n%var_names(j)) == 'foo') found_external = .true.
                    end do
                end if
            end select
        end do

        call expect(found_external, &
            '#2977: EXTERNAL statement in contained procedure dropped', failures)
    end subroutine check_2977_external_statement

end program test_issue_2966_silent_statement_drops
