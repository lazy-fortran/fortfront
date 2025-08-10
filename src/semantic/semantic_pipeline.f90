module semantic_pipeline
    use ast_core, only: ast_arena_t
    use semantic_analyzer_base, only: semantic_analyzer_t
    ! Import all analyzer types for safe allocation
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t
    use call_graph_analyzer, only: call_graph_analyzer_t
    use control_flow_analyzer, only: control_flow_analyzer_t
    use usage_tracker_analyzer, only: usage_tracker_analyzer_t
    use source_reconstruction_analyzer, only: source_reconstruction_analyzer_t
    use interface_analyzer, only: interface_analyzer_t
    use test_analyzer, only: simple_test_analyzer_t
    implicit none
    private

    public :: semantic_pipeline_t, analyzer_ptr
    public :: create_pipeline, destroy_pipeline

    ! Wrapper for analyzer pointers to allow arrays
    type :: analyzer_ptr
        class(semantic_analyzer_t), allocatable :: analyzer
    end type

    ! Pipeline manager for semantic analysis
    type :: semantic_pipeline_t
        type(analyzer_ptr), allocatable :: analyzers(:)
        integer :: analyzer_count = 0
    contains
        procedure :: register_analyzer
        procedure :: run_analysis
        procedure :: get_analyzer_count
        procedure :: get_analyzer
        procedure :: clear_analyzers
        final :: cleanup_pipeline
    end type

contains

    function create_pipeline() result(pipeline)
        type(semantic_pipeline_t) :: pipeline
        
        pipeline%analyzer_count = 0
        ! Initialize empty analyzers array
        allocate(pipeline%analyzers(0))
    end function

    subroutine destroy_pipeline(pipeline)
        type(semantic_pipeline_t), intent(inout) :: pipeline
        
        call pipeline%clear_analyzers()
        ! Shared context no longer used
    end subroutine

    subroutine register_analyzer(this, analyzer)
        class(semantic_pipeline_t), intent(inout) :: this
        class(semantic_analyzer_t), intent(in) :: analyzer
        
        type(analyzer_ptr), allocatable :: temp_analyzers(:)
        integer :: i
        
        ! Grow analyzers array
        allocate(temp_analyzers(this%analyzer_count + 1))
        
        ! Move existing analyzers (no copy!)
        do i = 1, this%analyzer_count
            call move_alloc(this%analyzers(i)%analyzer, temp_analyzers(i)%analyzer)
        end do
        
        ! SAFE: Use type-specific allocation and deep copy via assignment operator
        select type(a => analyzer)
        type is (symbol_analyzer_t)
            allocate(symbol_analyzer_t :: temp_analyzers(this%analyzer_count + 1)%analyzer)
        type is (type_analyzer_t)
            allocate(type_analyzer_t :: temp_analyzers(this%analyzer_count + 1)%analyzer)
        type is (scope_analyzer_t)
            allocate(scope_analyzer_t :: temp_analyzers(this%analyzer_count + 1)%analyzer)
        type is (call_graph_analyzer_t)
            allocate(call_graph_analyzer_t :: temp_analyzers(this%analyzer_count + 1)%analyzer)
        type is (control_flow_analyzer_t)
            allocate(control_flow_analyzer_t :: temp_analyzers(this%analyzer_count + 1)%analyzer)
        type is (usage_tracker_analyzer_t)
            allocate(usage_tracker_analyzer_t :: temp_analyzers(this%analyzer_count + 1)%analyzer)
        type is (source_reconstruction_analyzer_t)
            allocate(source_reconstruction_analyzer_t :: temp_analyzers(this%analyzer_count + 1)%analyzer)
        type is (interface_analyzer_t)
            allocate(interface_analyzer_t :: temp_analyzers(this%analyzer_count + 1)%analyzer)
        type is (simple_test_analyzer_t)
            allocate(simple_test_analyzer_t :: temp_analyzers(this%analyzer_count + 1)%analyzer)
        class default
            error stop "Unknown analyzer type in register_analyzer"
        end select
        
        ! Deep copy using overloaded assignment operator
        temp_analyzers(this%analyzer_count + 1)%analyzer = analyzer
        
        ! Update pipeline
        call move_alloc(temp_analyzers, this%analyzers)
        this%analyzer_count = this%analyzer_count + 1
    end subroutine

    subroutine run_analysis(this, arena, root_node_index)
        class(semantic_pipeline_t), intent(inout) :: this
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_node_index
        
        integer :: i
        integer :: dummy_context
        
        ! For now, use a simple non-allocatable context
        ! The allocatable class(*) was causing issues
        dummy_context = 0
        
        ! Run each registered analyzer
        do i = 1, this%analyzer_count
            if (allocated(this%analyzers(i)%analyzer)) then
                call this%analyzers(i)%analyzer%analyze( &
                    dummy_context, arena, root_node_index)
            end if
        end do
    end subroutine

    function get_analyzer_count(this) result(count)
        class(semantic_pipeline_t), intent(in) :: this
        integer :: count
        
        count = this%analyzer_count
    end function
    
    function get_analyzer(this, index) result(analyzer)
        class(semantic_pipeline_t), intent(in) :: this
        integer, intent(in) :: index
        class(semantic_analyzer_t), allocatable :: analyzer
        
        if (index > 0 .and. index <= this%analyzer_count) then
            if (allocated(this%analyzers(index)%analyzer)) then
                ! Type-specific allocation + assignment
                select type(a => this%analyzers(index)%analyzer)
                type is (simple_test_analyzer_t)
                    allocate(simple_test_analyzer_t :: analyzer)
                    ! Direct assignment for test analyzer
                    select type(analyzer)
                    type is (simple_test_analyzer_t)
                        analyzer = a
                    end select
                type is (symbol_analyzer_t)
                    allocate(symbol_analyzer_t :: analyzer)
                    select type(analyzer)
                    type is (symbol_analyzer_t)
                        analyzer = a
                    end select
                type is (type_analyzer_t)
                    allocate(type_analyzer_t :: analyzer)
                    select type(analyzer)
                    type is (type_analyzer_t)
                        analyzer = a
                    end select
                type is (scope_analyzer_t)
                    allocate(scope_analyzer_t :: analyzer)
                    select type(analyzer)
                    type is (scope_analyzer_t)
                        analyzer = a
                    end select
                type is (call_graph_analyzer_t)
                    allocate(call_graph_analyzer_t :: analyzer)
                    select type(analyzer)
                    type is (call_graph_analyzer_t)
                        analyzer = a
                    end select
                type is (control_flow_analyzer_t)
                    allocate(control_flow_analyzer_t :: analyzer)
                    select type(analyzer)
                    type is (control_flow_analyzer_t)
                        analyzer = a
                    end select
                type is (usage_tracker_analyzer_t)
                    allocate(usage_tracker_analyzer_t :: analyzer)
                    select type(analyzer)
                    type is (usage_tracker_analyzer_t)
                        analyzer = a
                    end select
                type is (source_reconstruction_analyzer_t)
                    allocate(source_reconstruction_analyzer_t :: analyzer)
                    select type(analyzer)
                    type is (source_reconstruction_analyzer_t)
                        analyzer = a
                    end select
                type is (interface_analyzer_t)
                    allocate(interface_analyzer_t :: analyzer)
                    select type(analyzer)
                    type is (interface_analyzer_t)
                        analyzer = a
                    end select
                class default
                    ! Should not happen if registered properly
                    return
                end select
            end if
        end if
    end function

    subroutine clear_analyzers(this)
        class(semantic_pipeline_t), intent(inout) :: this
        
        ! Deallocate the entire array - Fortran will handle component cleanup
        if (allocated(this%analyzers)) then
            deallocate(this%analyzers)
        end if
        allocate(this%analyzers(0))
        this%analyzer_count = 0
    end subroutine

    subroutine cleanup_pipeline(this)
        type(semantic_pipeline_t), intent(inout) :: this
        
        ! Deallocate analyzers directly without reallocating empty array
        if (allocated(this%analyzers)) then
            deallocate(this%analyzers)
        end if
        ! Shared context no longer used
    end subroutine

end module semantic_pipeline