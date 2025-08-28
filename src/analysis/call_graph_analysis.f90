module call_graph_analysis
    ! Call graph building and analysis functionality
    ! Provides APIs for building and querying call graphs
    
    use ast_core, only: ast_arena_t
    use call_graph_module, only: call_graph_t, call_edge_t, &
                                get_all_procedures, find_unused_procedures, &
                                get_callers, get_callees, find_recursive_cycles, &
                                cg_is_procedure_used => is_procedure_used
    use call_graph_builder_module, only: build_call_graph
    
    implicit none
    private
    
    ! Public call graph analysis functions
    public :: build_call_graph_from_arena, get_unused_procedures, &
              get_procedure_callers, get_procedure_callees, &
              is_procedure_used, get_all_procedures_in_graph, &
              get_call_edges, get_recursive_cycles
    
contains

    ! Build call graph from AST arena
    function build_call_graph_from_arena(arena, root_index) result(graph)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(call_graph_t) :: graph
        
        ! Use the existing call graph builder
        graph = build_call_graph(arena, root_index)
    end function build_call_graph_from_arena

    ! Get list of unused procedures in the call graph
    function get_unused_procedures(graph) result(proc_names)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: proc_names(:)
        
        proc_names = find_unused_procedures(graph)
    end function get_unused_procedures

    ! Get list of procedures that call the given procedure
    function get_procedure_callers(graph, procedure_name) result(caller_names)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: caller_names(:)
        
        caller_names = get_callers(graph, procedure_name)
    end function get_procedure_callers

    ! Get list of procedures called by the given procedure
    function get_procedure_callees(graph, procedure_name) result(callee_names)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        character(len=:), allocatable :: callee_names(:)
        
        callee_names = get_callees(graph, procedure_name)
    end function get_procedure_callees

    ! Check if a procedure is used in the call graph
    function is_procedure_used(graph, procedure_name) result(is_used)
        type(call_graph_t), intent(in) :: graph
        character(len=*), intent(in) :: procedure_name
        logical :: is_used
        
        is_used = cg_is_procedure_used(graph, procedure_name)
    end function is_procedure_used

    ! Get all procedures in the call graph
    function get_all_procedures_in_graph(graph) result(proc_names)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: proc_names(:)
        
        proc_names = get_all_procedures(graph)
    end function get_all_procedures_in_graph

    ! Get all call edges in the graph
    function get_call_edges(graph) result(edges)
        type(call_graph_t), intent(in) :: graph
        type(call_edge_t), allocatable :: edges(:)
        
        ! TODO: Implement edge extraction from call graph
        ! This would iterate through the graph structure and extract edges
        allocate(edges(0))
    end function get_call_edges

    ! Find recursive cycles in the call graph
    function get_recursive_cycles(graph) result(cycles)
        type(call_graph_t), intent(in) :: graph
        character(len=:), allocatable :: cycles(:)
        
        cycles = find_recursive_cycles(graph)
    end function get_recursive_cycles

end module call_graph_analysis