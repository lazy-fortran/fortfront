module semantic_pipeline
    ! Extensible semantic analysis pipeline for analyzer plugins
    use semantic_analyzer_base, only: semantic_analyzer_t
    use base_analyzer, only: base_analyzer_t, analysis_results_t
    use ast_core, only: ast_arena_t
    implicit none
    private
    
    public :: semantic_pipeline_t, analyzer_ptr, create_pipeline, destroy_pipeline
    
    ! Type for holding analyzer pointers
    type :: analyzer_ptr
        class(semantic_analyzer_t), allocatable :: analyzer
    contains
        procedure :: is_allocated => analyzer_ptr_is_allocated
        procedure :: assign => analyzer_ptr_assign
        generic :: assignment(=) => assign
    end type analyzer_ptr
    
    ! Semantic analysis pipeline
    type :: semantic_pipeline_t
        type(analyzer_ptr), allocatable :: analyzers(:)
        character(len=32), allocatable :: analyzer_names(:)
        integer :: count = 0
        logical :: initialized = .false.
    contains
        procedure :: add_analyzer => pipeline_add_analyzer
        procedure :: register_analyzer => pipeline_register_analyzer
        procedure :: run_analysis => pipeline_run_analysis
        procedure :: get_analyzer => pipeline_get_analyzer
        procedure :: get_analyzer_count => pipeline_get_analyzer_count
        procedure :: clear => pipeline_clear
        procedure :: assign => pipeline_assign
        generic :: assignment(=) => assign
    end type semantic_pipeline_t
    
contains
    
    ! Create a new semantic pipeline
    function create_pipeline() result(pipeline)
        type(semantic_pipeline_t) :: pipeline
        
        pipeline%count = 0
        pipeline%initialized = .true.
    end function create_pipeline
    
    ! Destroy semantic pipeline (alias for clear)
    subroutine destroy_pipeline(pipeline)
        type(semantic_pipeline_t), intent(inout) :: pipeline
        call pipeline%clear()
    end subroutine destroy_pipeline
    
    ! Register analyzer (get name from analyzer itself)
    subroutine pipeline_register_analyzer(this, analyzer)
        class(semantic_pipeline_t), intent(inout) :: this
        class(semantic_analyzer_t), intent(in) :: analyzer
        
        call this%add_analyzer(analyzer, analyzer%get_name())
    end subroutine pipeline_register_analyzer
    
    ! Add an analyzer to the pipeline
    subroutine pipeline_add_analyzer(this, analyzer, name)
        class(semantic_pipeline_t), intent(inout) :: this
        class(semantic_analyzer_t), intent(in) :: analyzer
        character(len=*), intent(in) :: name
        type(analyzer_ptr), allocatable :: temp_analyzers(:)
        character(len=32), allocatable :: temp_names(:)
        integer :: new_size
        
        ! Expand arrays if needed
        if (.not. allocated(this%analyzers)) then
            allocate(this%analyzers(10))
            allocate(this%analyzer_names(10))
        else if (this%count >= size(this%analyzers)) then
            new_size = size(this%analyzers) * 2
            
            ! Save existing data
            allocate(temp_analyzers(this%count))
            allocate(temp_names(this%count))
            temp_analyzers(1:this%count) = this%analyzers(1:this%count)
            temp_names(1:this%count) = this%analyzer_names(1:this%count)
            
            ! Reallocate with larger size
            deallocate(this%analyzers, this%analyzer_names)
            allocate(this%analyzers(new_size))
            allocate(this%analyzer_names(new_size))
            
            ! Restore data
            this%analyzers(1:this%count) = temp_analyzers
            this%analyzer_names(1:this%count) = temp_names
        end if
        
        ! Add new analyzer
        this%count = this%count + 1
        allocate(this%analyzers(this%count)%analyzer, source=analyzer)
        this%analyzer_names(this%count) = trim(name)
    end subroutine pipeline_add_analyzer
    
    ! Run analysis pipeline on AST
    subroutine pipeline_run_analysis(this, arena, node_index, shared_context)
        class(semantic_pipeline_t), intent(inout) :: this
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        class(*), intent(in), optional :: shared_context
        integer :: i
        
        if (.not. this%initialized .or. this%count == 0) return
        
        do i = 1, this%count
            if (this%analyzers(i)%is_allocated()) then
                if (present(shared_context)) then
                    call this%analyzers(i)%analyzer%analyze(shared_context, arena, &
                                                           node_index)
                else
                    ! Create default context
                    block
                        integer :: default_context
                        default_context = 0
                        call this%analyzers(i)%analyzer%analyze(default_context, &
                                                               arena, node_index)
                    end block
                end if
            end if
        end do
    end subroutine pipeline_run_analysis
    
    ! Get analyzer by index
    function pipeline_get_analyzer(this, index) result(analyzer)
        class(semantic_pipeline_t), intent(in) :: this
        integer, intent(in) :: index
        class(semantic_analyzer_t), pointer :: analyzer
        
        analyzer => null()
        if (index > 0 .and. index <= this%count) then
            if (this%analyzers(index)%is_allocated()) then
                analyzer => this%analyzers(index)%analyzer
            end if
        end if
    end function pipeline_get_analyzer
    
    ! Get analyzer count
    function pipeline_get_analyzer_count(this) result(count)
        class(semantic_pipeline_t), intent(in) :: this
        integer :: count
        count = this%count
    end function pipeline_get_analyzer_count
    
    ! Clear pipeline
    subroutine pipeline_clear(this)
        class(semantic_pipeline_t), intent(inout) :: this
        integer :: i
        
        if (allocated(this%analyzers)) then
            do i = 1, this%count
                if (this%analyzers(i)%is_allocated()) then
                    deallocate(this%analyzers(i)%analyzer)
                end if
            end do
            deallocate(this%analyzers)
        end if
        
        if (allocated(this%analyzer_names)) deallocate(this%analyzer_names)
        
        this%count = 0
        this%initialized = .false.
    end subroutine pipeline_clear
    
    ! Assignment operator for pipeline
    subroutine pipeline_assign(lhs, rhs)
        class(semantic_pipeline_t), intent(out) :: lhs
        type(semantic_pipeline_t), intent(in) :: rhs
        integer :: i
        
        call lhs%clear()
        
        if (rhs%initialized .and. rhs%count > 0) then
            lhs%count = rhs%count
            lhs%initialized = rhs%initialized
            
            allocate(lhs%analyzers(rhs%count))
            allocate(lhs%analyzer_names(rhs%count))
            
            do i = 1, rhs%count
                if (rhs%analyzers(i)%is_allocated()) then
                    allocate(lhs%analyzers(i)%analyzer, &
                            source=rhs%analyzers(i)%analyzer)
                end if
                lhs%analyzer_names(i) = rhs%analyzer_names(i)
            end do
        end if
    end subroutine pipeline_assign
    
    ! Analyzer pointer helper methods
    function analyzer_ptr_is_allocated(this) result(is_alloc)
        class(analyzer_ptr), intent(in) :: this
        logical :: is_alloc
        is_alloc = allocated(this%analyzer)
    end function analyzer_ptr_is_allocated
    
    subroutine analyzer_ptr_assign(lhs, rhs)
        class(analyzer_ptr), intent(out) :: lhs
        type(analyzer_ptr), intent(in) :: rhs
        
        if (rhs%is_allocated()) then
            allocate(lhs%analyzer, source=rhs%analyzer)
        end if
    end subroutine analyzer_ptr_assign
    
end module semantic_pipeline