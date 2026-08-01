program test_issue_2984_construct_names
    ! fortfront #2984: a named IF, SELECT, ASSOCIATE or BLOCK lost its
    ! construct name. DO already kept one, so the source round-tripped with
    ! some constructs renamed and others not.
    !
    ! This is not only a fidelity problem. EXIT and CYCLE name the construct
    ! they target, so with no name on the node a consumer cannot resolve which
    ! construct `exit outer` leaves, nor check that it names an enclosing one.
    !
    ! Oracle: examples/f90/named_constructs.f90 is accepted by
    ! "gfortran -fsyntax-only" and names every construct that may carry a name.
    ! Round-tripping it must preserve each name on both the opening statement
    ! and its END, and the public query must report the same names.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    use frontend_core, only: lex_source
    use frontend_parsing, only: parse_tokens
    use fortfront, only: get_construct_name
    use lexer_core, only: token_t
    use ast_arena_modern, only: ast_arena_t, create_ast_arena
    use ast_nodes_conditional, only: if_node, select_case_node
    use ast_nodes_associate, only: associate_node, block_construct_node
    use ast_nodes_loops, only: do_loop_node
    use ast_nodes_transfer, only: exit_node, cycle_node
    implicit none

    integer :: failures

    failures = 0

    call check_round_trip(failures)
    call check_query_and_transfer_targets(failures)

    if (failures > 0) then
        write (error_unit, '(a,i0,a)') 'FAIL: ', failures, ' construct-name checks'
        error stop 1
    end if
    print *, 'PASS: construct names survive parse, codegen and the query API'

contains

    include '../common/read_example.inc'

    ! Every named construct must round-trip with its name on both statements.
    subroutine check_round_trip(failures)
        integer, intent(inout) :: failures
        character(len=:), allocatable :: source, output, errors

        call read_example('examples/f90/named_constructs.f90', source)
        call transform_lazy_fortran_string(source, output, errors)
        if (allocated(errors)) then
            if (len_trim(errors) > 0) then
                write (error_unit, '(a)') 'FAIL: transform reported: '//trim(errors)
                failures = failures + 1
                return
            end if
        end if

        call expect_contains(output, 'outer: do', failures)
        call expect_contains(output, 'end do outer', failures)
        call expect_contains(output, 'inner: do', failures)
        call expect_contains(output, 'end do inner', failures)
        call expect_contains(output, 'check: if', failures)
        call expect_contains(output, 'end if check', failures)
        call expect_contains(output, 'pick: select case', failures)
        call expect_contains(output, 'end select pick', failures)
        call expect_contains(output, 'scope: block', failures)
        call expect_contains(output, 'end block scope', failures)
        call expect_contains(output, 'link: associate', failures)
        call expect_contains(output, 'end associate link', failures)
        ! EXIT and CYCLE keep naming the construct they target.
        call expect_contains(output, 'cycle outer', failures)
        call expect_contains(output, 'exit outer', failures)
    end subroutine check_round_trip

    ! The public query reports the name for each construct kind, reports an
    ! empty string for an unnamed one, and lets a consumer match an EXIT or
    ! CYCLE label against an enclosing construct -- here from inside a nested
    ! construct, where `exit outer` skips past `inner`.
    subroutine check_query_and_transfer_targets(failures)
        integer, intent(inout) :: failures
        character(len=:), allocatable :: source
        character(len=1024) :: error_msg
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: lex_error
        type(ast_arena_t) :: arena
        integer :: prog_index, i
        logical :: saw_if, saw_select, saw_block, saw_associate
        logical :: saw_unnamed_if, resolved_exit, resolved_cycle
        character(len=:), allocatable :: outer_name

        call read_example('examples/f90/named_constructs.f90', source)
        arena = create_ast_arena()
        call lex_source(source, tokens, lex_error)
        error_msg = ''
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') 'FAIL: parse reported: '//trim(error_msg)
            failures = failures + 1
            return
        end if

        saw_if = .false.
        saw_select = .false.
        saw_block = .false.
        saw_associate = .false.
        saw_unnamed_if = .false.
        outer_name = ''
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (n => arena%entries(i)%node)
            type is (if_node)
                if (get_construct_name(arena, i) == 'check') saw_if = .true.
                if (get_construct_name(arena, i) == '') saw_unnamed_if = .true.
            type is (select_case_node)
                if (get_construct_name(arena, i) == 'pick') saw_select = .true.
            type is (block_construct_node)
                if (get_construct_name(arena, i) == 'scope') saw_block = .true.
            type is (associate_node)
                if (get_construct_name(arena, i) == 'link') saw_associate = .true.
            type is (do_loop_node)
                if (get_construct_name(arena, i) == 'outer') &
                    outer_name = get_construct_name(arena, i)
            end select
        end do

        call expect(saw_if, 'named IF reports its construct name', failures)
        call expect(saw_select, 'named SELECT CASE reports its construct name', &
            failures)
        call expect(saw_block, 'named BLOCK reports its construct name', failures)
        call expect(saw_associate, 'named ASSOCIATE reports its construct name', &
            failures)
        call expect(saw_unnamed_if, &
            'an unnamed IF reports an empty construct name', failures)
        call expect(outer_name == 'outer', &
            'named DO reports its construct name through the same query', &
            failures)

        ! Resolve the EXIT and CYCLE that target the OUTER loop from inside
        ! the nested INNER loop: their label must match a construct name that
        ! the query reports.
        resolved_exit = .false.
        resolved_cycle = .false.
        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (n => arena%entries(i)%node)
            type is (exit_node)
                if (allocated(n%label)) then
                    if (trim(n%label) == outer_name) resolved_exit = .true.
                end if
            type is (cycle_node)
                if (allocated(n%label)) then
                    if (trim(n%label) == outer_name) resolved_cycle = .true.
                end if
            end select
        end do

        call expect(resolved_exit, &
            'EXIT from a nested construct resolves to the named outer DO', &
            failures)
        call expect(resolved_cycle, &
            'CYCLE from a nested construct resolves to the named outer DO', &
            failures)
    end subroutine check_query_and_transfer_targets

    subroutine expect_contains(haystack, needle, failures)
        character(len=*), intent(in) :: haystack, needle
        integer, intent(inout) :: failures

        if (index(haystack, needle) == 0) then
            write (error_unit, '(a)') 'FAIL: round-trip lost "'//needle//'"'
            failures = failures + 1
        end if
    end subroutine expect_contains

    subroutine expect(condition, label, failures)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: label
        integer, intent(inout) :: failures

        if (.not. condition) then
            write (error_unit, '(a)') 'FAIL: '//label
            failures = failures + 1
        end if
    end subroutine expect

end program test_issue_2984_construct_names
