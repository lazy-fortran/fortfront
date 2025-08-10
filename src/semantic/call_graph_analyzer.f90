module call_graph_analyzer
    use semantic_analyzer_base, only: semantic_analyzer_t
    use ast_core, only: ast_arena_t
    use call_graph_module, only: call_graph_t, create_call_graph, &
                                 procedure_info_t, call_edge_t
    use call_graph_builder_module, only: build_call_graph
    implicit none
    private

    public :: call_graph_analyzer_t

    ! Call graph analyzer plugin
    type, extends(semantic_analyzer_t) :: call_graph_analyzer_t
        type(call_graph_t) :: call_graph
        logical :: analysis_complete = .false.
    contains
        procedure :: analyze => analyze_call_graph
        procedure :: get_results => get_call_graph_results
        procedure :: get_name => get_call_graph_analyzer_name
        
        ! Analysis methods for fluff rules
        procedure :: find_unused_procedures
        procedure :: find_recursive_cycles
        procedure :: get_call_chain_depth
        procedure :: is_procedure_used
        procedure :: get_all_procedures
    end type

contains

    subroutine analyze_call_graph(this, shared_context, arena, node_index)
        class(call_graph_analyzer_t), intent(inout) :: this
        class(*), intent(in) :: shared_context
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Build call graph from AST using the function interface
        this%call_graph = build_call_graph(arena, node_index)
        
        this%analysis_complete = .true.
    end subroutine

    function get_call_graph_results(this) result(results)
        class(call_graph_analyzer_t), intent(in) :: this
        class(*), allocatable :: results
        
        ! Return the call graph
        allocate(call_graph_t :: results)
        select type(results)
        type is (call_graph_t)
            results = this%call_graph
        end select
    end function

    function get_call_graph_analyzer_name(this) result(name)
        class(call_graph_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "call_graph_analyzer"
    end function

    ! Analysis methods for fluff rules
    function find_unused_procedures(this) result(unused_procs)
        class(call_graph_analyzer_t), intent(in) :: this
        character(:), allocatable :: unused_procs(:)
        
        if (.not. this%analysis_complete) then
            allocate(character(0) :: unused_procs(0))
            return
        end if
        
        ! Use existing call graph functionality
        unused_procs = this%call_graph%find_unused()
    end function

    function find_recursive_cycles(this) result(recursive_cycles)
        class(call_graph_analyzer_t), intent(in) :: this
        character(:), allocatable :: recursive_cycles(:)
        
        if (.not. this%analysis_complete) then
            allocate(character(0) :: recursive_cycles(0))
            return
        end if
        
        ! Use existing call graph functionality - for now return empty
        ! (would call actual find_cycles method if it exists)
        allocate(character(0) :: recursive_cycles(0))
    end function

    function get_call_chain_depth(this, procedure_name) result(depth)
        class(call_graph_analyzer_t), intent(in) :: this
        character(*), intent(in) :: procedure_name
        integer :: depth
        
        if (.not. this%analysis_complete) then
            depth = 0
            return
        end if
        
        ! Calculate maximum call chain depth for performance analysis
        depth = calculate_max_depth(this%call_graph, procedure_name)
    end function

    function is_procedure_used(this, procedure_name) result(used)
        class(call_graph_analyzer_t), intent(in) :: this
        character(*), intent(in) :: procedure_name
        logical :: used
        
        if (.not. this%analysis_complete) then
            used = .false.
            return
        end if
        
        used = this%call_graph%is_used(procedure_name)
    end function

    function get_all_procedures(this) result(procedures)
        class(call_graph_analyzer_t), intent(in) :: this
        type(procedure_info_t), allocatable :: procedures(:)
        
        if (.not. this%analysis_complete) then
            allocate(procedures(0))
            return
        end if
        
        ! Return all procedures from call graph
        procedures = this%call_graph%procedures(1:this%call_graph%proc_count)
    end function

    ! Helper function for call chain depth calculation
    recursive function calculate_max_depth(call_graph, start_proc) result(max_depth)
        type(call_graph_t), intent(in) :: call_graph
        character(*), intent(in) :: start_proc
        integer :: max_depth
        
        integer :: i, current_depth
        character(:), allocatable :: callees(:)
        
        max_depth = 0
        callees = call_graph%get_proc_callees(start_proc)
        
        do i = 1, size(callees)
            current_depth = 1 + calculate_max_depth(call_graph, callees(i))
            max_depth = max(max_depth, current_depth)
        end do
    end function

end module call_graph_analyzer