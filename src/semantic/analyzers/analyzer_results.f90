module analyzer_results
    ! SAFE: All result types in one place, no polymorphism needed
    use call_graph_module, only: call_graph_t
    use control_flow_graph_module, only: control_flow_graph_t
    use variable_usage_tracker_module, only: variable_usage_info_t
    implicit none
    private

    public :: analyzer_results_t
    public :: clear_results

    ! Union-like type containing all possible results
    type :: analyzer_results_t
        ! Each analyzer stores its results here
        type(call_graph_t), allocatable :: call_graph
        type(control_flow_graph_t), allocatable :: control_flow
        type(variable_usage_info_t), allocatable :: usage_info
        
        ! Simple results that don't need complex types
        character(:), allocatable :: unused_variables(:)
        character(:), allocatable :: undefined_variables(:)
        character(:), allocatable :: unused_procedures(:)
        integer, allocatable :: unreachable_nodes(:)
        
        ! Which results are valid
        logical :: has_call_graph = .false.
        logical :: has_control_flow = .false.
        logical :: has_usage_info = .false.
    end type

contains

    subroutine clear_results(results)
        type(analyzer_results_t), intent(inout) :: results
        
        if (allocated(results%call_graph)) deallocate(results%call_graph)
        if (allocated(results%control_flow)) deallocate(results%control_flow)
        if (allocated(results%usage_info)) deallocate(results%usage_info)
        if (allocated(results%unused_variables)) deallocate(results%unused_variables)
        if (allocated(results%undefined_variables)) &
            deallocate(results%undefined_variables)
        if (allocated(results%unused_procedures)) deallocate(results%unused_procedures)
        if (allocated(results%unreachable_nodes)) deallocate(results%unreachable_nodes)
        
        results%has_call_graph = .false.
        results%has_control_flow = .false.
        results%has_usage_info = .false.
    end subroutine

end module analyzer_results