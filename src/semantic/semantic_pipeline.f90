module semantic_pipeline
    use ast_core, only: ast_arena_t
    use semantic_analyzer_base, only: semantic_analyzer_t
    use dependency_graph, only: dependency_graph_t, create_dependency_graph
    ! No longer need hard-coded analyzer imports thanks to polymorphic allocation
    implicit none
    private

    public :: semantic_pipeline_t, analyzer_ptr, analysis_result_t, shared_context_t
    public :: create_pipeline, destroy_pipeline, create_shared_context

    ! Wrapper for analyzer pointers to allow arrays
    type :: analyzer_ptr
        class(semantic_analyzer_t), allocatable :: analyzer
    end type

    ! Analysis result storage
    type :: analysis_result_t
        character(len=32) :: analyzer_name = ""
        class(*), allocatable :: result_data
    end type

    ! Shared context for multi-pass analysis
    type :: shared_context_t
        type(analysis_result_t), allocatable :: results(:)
        integer :: result_count = 0
    contains
        procedure :: store_result
        procedure :: get_result
        procedure :: has_result
        procedure :: clear_results
    end type

    ! Pipeline manager for semantic analysis
    type :: semantic_pipeline_t
        type(analyzer_ptr), allocatable :: analyzers(:)
        integer :: analyzer_count = 0
        type(dependency_graph_t) :: dep_graph
        type(shared_context_t) :: context
    contains
        procedure :: register_analyzer
        procedure :: run_analysis
        procedure :: get_analyzer_count
        procedure :: get_analyzer
        procedure :: clear_analyzers
        procedure :: find_analyzer_by_name
        final :: cleanup_pipeline
    end type

contains

    function create_pipeline() result(pipeline)
        type(semantic_pipeline_t) :: pipeline
        
        pipeline%analyzer_count = 0
        ! Initialize empty analyzers array
        allocate(pipeline%analyzers(0))
        
        ! Initialize dependency graph and shared context
        pipeline%dep_graph = create_dependency_graph()
        pipeline%context = create_shared_context()
    end function

    function create_shared_context() result(context)
        type(shared_context_t) :: context
        
        context%result_count = 0
        allocate(context%results(0))
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
        
        ! SOLUTION: Use polymorphic allocation to eliminate hard-coded type dependencies
        ! This resolves the dependency injection violation by accepting any semantic_analyzer_t
        ! subtype without the pipeline needing to know about specific analyzer implementations.
        allocate(temp_analyzers(this%analyzer_count + 1)%analyzer, source=analyzer)
        
        ! Deep copy using overloaded assignment operator  
        temp_analyzers(this%analyzer_count + 1)%analyzer = analyzer
        
        ! Update pipeline
        call move_alloc(temp_analyzers, this%analyzers)
        this%analyzer_count = this%analyzer_count + 1
        
        ! Add to dependency graph
        call this%dep_graph%add_node(analyzer%get_name(), analyzer%get_dependencies())
    end subroutine

    subroutine run_analysis(this, arena, root_node_index)
        class(semantic_pipeline_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_node_index
        
        character(len=32), allocatable :: execution_order(:)
        integer :: i, analyzer_index
        
        ! Get dependency-ordered execution sequence
        execution_order = this%dep_graph%get_execution_order()
        
        ! Run analyzers in dependency order
        do i = 1, size(execution_order)
            analyzer_index = this%find_analyzer_by_name(execution_order(i))
            if (analyzer_index > 0) then
                if (allocated(this%analyzers(analyzer_index)%analyzer)) then
                    ! Use shared context for multi-pass analysis
                    call this%analyzers(analyzer_index)%analyzer%analyze( &
                        this%context, arena, root_node_index)
                    
                    ! Safe transfer of results to shared context
                    ! Use block scope to ensure proper cleanup
                    block
                        class(*), allocatable :: temp_results
                        temp_results = &
                            this%analyzers(analyzer_index)%analyzer%get_results()
                        if (allocated(temp_results)) then
                            call this%context%store_result(execution_order(i), temp_results)
                        end if
                        ! temp_results automatically deallocated at end of block
                    end block
                end if
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
                ! Simple polymorphic copy using source allocation
                allocate(analyzer, source=this%analyzers(index)%analyzer)
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

    function find_analyzer_by_name(this, name) result(index)
        class(semantic_pipeline_t), intent(in) :: this
        character(*), intent(in) :: name
        integer :: index
        
        integer :: i
        
        index = 0
        do i = 1, this%analyzer_count
            if (allocated(this%analyzers(i)%analyzer)) then
                if (trim(this%analyzers(i)%analyzer%get_name()) == trim(name)) then
                    index = i
                    return
                end if
            end if
        end do
    end function

    subroutine cleanup_pipeline(this)
        type(semantic_pipeline_t), intent(inout) :: this
        
        ! Deallocate analyzers directly without reallocating empty array
        if (allocated(this%analyzers)) then
            deallocate(this%analyzers)
        end if
        ! Shared context cleanup handled automatically
    end subroutine

    ! Shared context methods
    subroutine store_result(this, analyzer_name, result_data)
        use semantic_analyzer, only: semantic_context_t
        class(shared_context_t), intent(inout) :: this
        character(*), intent(in) :: analyzer_name
        class(*), intent(in) :: result_data
        
        type(analysis_result_t), allocatable :: temp_results(:)
        integer :: i
        
        ! Grow results array
        allocate(temp_results(this%result_count + 1))
        
        ! Copy existing results
        do i = 1, this%result_count
            temp_results(i)%analyzer_name = this%results(i)%analyzer_name
            if (allocated(this%results(i)%result_data)) then
                ! Safe copy for all supported types
                select type(src => this%results(i)%result_data)
                type is (integer)
                    allocate(integer :: temp_results(i)%result_data)
                    select type(dst => temp_results(i)%result_data)
                    type is (integer)
                        dst = src
                    end select
                type is (logical)
                    allocate(logical :: temp_results(i)%result_data)
                    select type(dst => temp_results(i)%result_data)
                    type is (logical)
                        dst = src
                    end select
                type is (semantic_context_t)
                    allocate(semantic_context_t :: temp_results(i)%result_data)
                    select type(dst => temp_results(i)%result_data)
                    type is (semantic_context_t)
                        ! Use proper assignment operator for deep copy
                        dst = src
                    end select
                class default
                    ! For unknown types, use source allocation (may be unsafe)
                    allocate(temp_results(i)%result_data, source=src)
                end select
            end if
        end do
        
        ! Add new result
        temp_results(this%result_count + 1)%analyzer_name = trim(analyzer_name)
        select type(src => result_data)
        type is (integer)
            allocate(integer :: temp_results(this%result_count + 1)%result_data)
            select type(dst => temp_results(this%result_count + 1)%result_data)
            type is (integer)
                dst = src
            end select
        type is (logical)
            allocate(logical :: temp_results(this%result_count + 1)%result_data)
            select type(dst => temp_results(this%result_count + 1)%result_data)
            type is (logical)
                dst = src
            end select
        type is (semantic_context_t)
            allocate(semantic_context_t :: temp_results(this%result_count + 1)%result_data)
            select type(dst => temp_results(this%result_count + 1)%result_data)
            type is (semantic_context_t)
                ! Use proper assignment operator for deep copy
                dst = src
            end select
        class default
            ! For unknown types, use source allocation (may be unsafe)
            allocate(temp_results(this%result_count + 1)%result_data, source=src)
        end select
        
        ! Update context
        call move_alloc(temp_results, this%results)
        this%result_count = this%result_count + 1
    end subroutine

    function get_result(this, analyzer_name) result(result_data)
        use semantic_analyzer, only: semantic_context_t
        class(shared_context_t), intent(in) :: this
        character(*), intent(in) :: analyzer_name
        class(*), allocatable :: result_data
        
        integer :: i
        
        do i = 1, this%result_count
            if (trim(this%results(i)%analyzer_name) == trim(analyzer_name)) then
                if (allocated(this%results(i)%result_data)) then
                    ! Safe copy for all supported types
                    select type(src => this%results(i)%result_data)
                    type is (integer)
                        allocate(integer :: result_data)
                        select type(dst => result_data)
                        type is (integer)
                            dst = src
                        end select
                    type is (logical)
                        allocate(logical :: result_data)
                        select type(dst => result_data)
                        type is (logical)
                            dst = src
                        end select
                    type is (semantic_context_t)
                        allocate(semantic_context_t :: result_data)
                        select type(dst => result_data)
                        type is (semantic_context_t)
                            ! Use proper assignment operator for deep copy
                            dst = src
                        end select
                    class default
                        ! For unknown types, use source allocation (may be unsafe)
                        allocate(result_data, source=src)
                    end select
                end if
                return
            end if
        end do
        
        ! Result not found - return unallocated
    end function

    function has_result(this, analyzer_name) result(found)
        class(shared_context_t), intent(in) :: this
        character(*), intent(in) :: analyzer_name
        logical :: found
        
        integer :: i
        
        found = .false.
        do i = 1, this%result_count
            if (trim(this%results(i)%analyzer_name) == trim(analyzer_name)) then
                found = .true.
                return
            end if
        end do
    end function

    subroutine clear_results(this)
        class(shared_context_t), intent(inout) :: this
        
        if (allocated(this%results)) then
            deallocate(this%results)
        end if
        allocate(this%results(0))
        this%result_count = 0
    end subroutine

end module semantic_pipeline