module call_graph_module
    ! Public facade over the split call graph implementation modules. Consumers
    ! import this module instead of depending on the internal module layout.
    use call_graph_core_mod, only: call_graph_t, procedure_info_t, call_edge_t, &
        create_call_graph
    use call_graph_queries_mod, only: get_callers, get_callees, is_procedure_used, &
        get_all_procedures, get_call_count
    use call_graph_builder_mod, only: build_call_graph_from_ast, build_call_graph
    implicit none
    private

    public :: call_graph_t, procedure_info_t, call_edge_t
    public :: create_call_graph
    public :: build_call_graph_from_ast, build_call_graph
    public :: get_callers, get_callees, is_procedure_used
    public :: get_all_procedures, get_call_count

end module call_graph_module
