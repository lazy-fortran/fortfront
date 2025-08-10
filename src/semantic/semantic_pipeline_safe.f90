module semantic_pipeline_safe
    ! SAFE VERSION: Uses indices instead of polymorphic copies
    use ast_core, only: ast_arena_t
    use semantic_analyzer_base, only: semantic_analyzer_t
    implicit none
    private

    public :: semantic_pipeline_safe_t
    public :: register_analyzer_safe, create_pipeline_safe

    ! Global analyzer registry - analyzers live here permanently
    integer, parameter :: MAX_ANALYZERS = 100
    type :: analyzer_registry_t
        class(semantic_analyzer_t), allocatable :: analyzers(MAX_ANALYZERS)
        integer :: count = 0
    end type
    type(analyzer_registry_t), save :: global_registry

    ! Pipeline just holds indices into the global registry
    type :: semantic_pipeline_safe_t
        integer, allocatable :: analyzer_indices(:)  ! Just indices, no polymorphic copies!
        integer :: analyzer_count = 0
    contains
        procedure :: register_analyzer_index
        procedure :: run_analysis
        procedure :: get_analyzer_count
    end type

contains

    function create_pipeline_safe() result(pipeline)
        type(semantic_pipeline_safe_t) :: pipeline
        
        pipeline%analyzer_count = 0
        allocate(pipeline%analyzer_indices(0))
    end function

    ! Register analyzer in global registry and return index
    function register_analyzer_safe(analyzer) result(index)
        class(semantic_analyzer_t), intent(in) :: analyzer
        integer :: index
        
        if (global_registry%count >= MAX_ANALYZERS) then
            error stop "Analyzer registry full"
        end if
        
        global_registry%count = global_registry%count + 1
        index = global_registry%count
        
        ! UNSAFE: Still uses source= but only happens once per analyzer type
        ! Better would be to have concrete registration functions
        allocate(global_registry%analyzers(index), source=analyzer)
    end function

    subroutine register_analyzer_index(this, analyzer_index)
        class(semantic_pipeline_safe_t), intent(inout) :: this
        integer, intent(in) :: analyzer_index
        
        integer, allocatable :: temp_indices(:)
        
        ! Just copy integer indices - completely safe!
        allocate(temp_indices(this%analyzer_count + 1))
        if (this%analyzer_count > 0) then
            temp_indices(1:this%analyzer_count) = this%analyzer_indices
        end if
        temp_indices(this%analyzer_count + 1) = analyzer_index
        
        call move_alloc(temp_indices, this%analyzer_indices)
        this%analyzer_count = this%analyzer_count + 1
    end subroutine

    subroutine run_analysis(this, arena, root_node_index)
        class(semantic_pipeline_safe_t), intent(inout) :: this
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_node_index
        
        integer :: i, idx
        
        ! Run each analyzer by index
        do i = 1, this%analyzer_count
            idx = this%analyzer_indices(i)
            if (allocated(global_registry%analyzers(idx))) then
                ! No copying - just use the analyzer in place
                call global_registry%analyzers(idx)%analyze( &
                    arena, arena, root_node_index)
            end if
        end do
    end subroutine

    function get_analyzer_count(this) result(count)
        class(semantic_pipeline_safe_t), intent(in) :: this
        integer :: count
        
        count = this%analyzer_count
    end function

end module semantic_pipeline_safe