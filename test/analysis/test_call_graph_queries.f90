program test_call_graph_queries
    use fortfront, only: ast_arena_t, build_call_graph, call_graph_t, &
                         create_ast_arena, get_all_procedures, get_call_count, &
                         get_callees, get_callers, is_procedure_used, lex_source, &
                         parse_tokens, token_t
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    logical :: all_passed

    all_passed = .true.

    call test_mutual_recursion_edges()
    call test_unused_procedure_query()
    call test_caller_callee_queries()
    call test_empty_graph_queries()

    if (all_passed) then
        print '(a)', "All call graph query tests passed"
        stop 0
    end if

    print '(a)', "Some call graph query tests failed"
    stop 1

contains

    include '../common/read_example.inc'

    subroutine test_mutual_recursion_edges()
        type(call_graph_t) :: graph

        call parse_example('examples/f90/call_graph_mutual_recursion.f90', graph)

        call assert_edge(graph, 'call_graph_mutual_recursion_mod::first', &
                         'call_graph_mutual_recursion_mod::second')
        call assert_edge(graph, 'call_graph_mutual_recursion_mod::second', &
                         'call_graph_mutual_recursion_mod::first')
    end subroutine test_mutual_recursion_edges

    subroutine test_unused_procedure_query()
        type(call_graph_t) :: graph

        call parse_example('examples/f90/call_graph_unused_procedure.f90', graph)

        if (.not. is_procedure_used(graph, 'used_value')) then
            call report_failure('used procedure reported as unused')
        end if
        if (is_procedure_used(graph, 'unused_value')) then
            call report_failure('unused procedure reported as used')
        end if
    end subroutine test_unused_procedure_query

    subroutine test_caller_callee_queries()
        type(call_graph_t) :: graph
        character(len=:), allocatable :: callers(:), callees(:)

        call parse_example('examples/f90/call_graph_mutual_recursion.f90', graph)

        callers = get_callers(graph, 'first')
        if (.not. names_contains(callers, 'second')) then
            call report_failure('expected second among callers of first')
        end if

        callees = get_callees(graph, 'first')
        if (.not. names_contains(callees, 'second')) then
            call report_failure('expected second among callees of first')
        end if

        if (get_call_count(graph) < 2) then
            call report_failure('expected at least 2 calls in mutual recursion')
        end if
    end subroutine test_caller_callee_queries

    ! Regression for #2831: queries on a default-initialized graph (calls and
    ! procedures arrays unallocated, counts zero) must not touch unallocated
    ! arrays.
    subroutine test_empty_graph_queries()
        type(call_graph_t) :: graph
        character(len=:), allocatable :: names(:)

        names = get_callers(graph, 'anything')
        if (size(names) /= 0) then
            call report_failure('empty graph reported callers')
        end if

        names = get_callees(graph, 'anything')
        if (size(names) /= 0) then
            call report_failure('empty graph reported callees')
        end if

        names = get_all_procedures(graph)
        if (size(names) /= 0) then
            call report_failure('empty graph reported procedures')
        end if

        if (is_procedure_used(graph, 'anything')) then
            call report_failure('empty graph reported a used procedure')
        end if

        if (get_call_count(graph) /= 0) then
            call report_failure('empty graph reported nonzero call count')
        end if
    end subroutine test_empty_graph_queries

    logical function names_contains(names, target)
        character(len=*), intent(in) :: names(:)
        character(len=*), intent(in) :: target
        integer :: i

        names_contains = .false.
        do i = 1, size(names)
            if (trim(names(i)) == trim(target)) then
                names_contains = .true.
                return
            end if
        end do
    end function names_contains

    subroutine parse_example(example_path, graph)
        character(len=*), intent(in) :: example_path
        type(call_graph_t), intent(out) :: graph
        character(len=:), allocatable :: source
        character(len=:), allocatable :: error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index

        call read_example(example_path, source)
        call lex_source(source, tokens, error_msg)
        if (len_trim(error_msg) > 0) then
            call report_failure('lexing failed for ' // trim(example_path) // &
                                ': ' // trim(error_msg))
            return
        end if

        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        if (root_index <= 0) then
            call report_failure('parsing failed for ' // trim(example_path) // &
                                ': ' // trim(error_msg))
            return
        end if

        graph = build_call_graph(arena, root_index)
    end subroutine parse_example

    subroutine assert_edge(graph, caller, callee)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: caller
        character(len=*), intent(in) :: callee

        if (.not. edge_exists(graph, caller, callee)) then
            call report_failure('missing call edge: ' // trim(caller) // &
                                ' -> ' // trim(callee))
        end if
    end subroutine assert_edge

    logical function edge_exists(graph, caller, callee)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: caller
        character(len=*), intent(in) :: callee
        integer :: i

        edge_exists = .false.

        do i = 1, graph%call_count
            if (trim(graph%calls(i)%caller) == trim(caller) .and. &
                trim(graph%calls(i)%callee) == trim(callee)) then
                edge_exists = .true.
                return
            end if
        end do
    end function edge_exists

    subroutine report_failure(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(a)') 'FAIL: ' // trim(message)
        all_passed = .false.
    end subroutine report_failure

end program test_call_graph_queries
