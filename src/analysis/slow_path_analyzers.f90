! @slow-path
module slow_path_analyzers
    use ast_arena_modern, only: ast_arena_t
    use call_graph_module, only: call_graph_t, build_call_graph
    implicit none
    private

    type(call_graph_t), save :: cached_call_graph
    logical, save :: call_graph_available = .false.
    integer, save :: invocation_count = 0

    public :: run_slow_path_analyzers
    public :: clear_slow_path_results
    public :: fetch_call_graph_result
    public :: get_slow_path_invocation_count

contains

    subroutine run_slow_path_analyzers(arena, root_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(call_graph_t) :: graph

        invocation_count = invocation_count + 1
        graph = build_call_graph(arena, root_index)
        cached_call_graph = graph
        call_graph_available = .true.
    end subroutine run_slow_path_analyzers

    subroutine clear_slow_path_results()
        call_graph_available = .false.
        invocation_count = 0
    end subroutine clear_slow_path_results

    subroutine fetch_call_graph_result(graph, available)
        type(call_graph_t), intent(out) :: graph
        logical, intent(out) :: available

        if (.not. call_graph_available) then
            available = .false.
            return
        end if

        graph = cached_call_graph
        available = .true.
    end subroutine fetch_call_graph_result

    integer function get_slow_path_invocation_count() result(count)
        count = invocation_count
    end function get_slow_path_invocation_count

end module slow_path_analyzers
