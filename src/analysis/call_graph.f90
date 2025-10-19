module call_graph_module
    use call_graph_core_mod, only: call_graph_t, procedure_info_t, call_edge_t, &
                                   create_call_graph
    use call_graph_queries_mod, only: find_unused_procedures, get_callers, &
                                      get_callees, is_procedure_used, &
                                      get_all_procedures, get_call_count, &
                                      print_call_graph, find_recursive_cycles
    use call_graph_builder_mod, only: build_call_graph_from_ast, build_call_graph
    implicit none
    private

    public :: call_graph_t, procedure_info_t, call_edge_t
    public :: create_call_graph
    public :: build_call_graph_from_ast, build_call_graph
    public :: find_unused_procedures, get_callers, get_callees, is_procedure_used
    public :: get_all_procedures, get_call_count, find_recursive_cycles
    public :: print_call_graph

end module call_graph_module
