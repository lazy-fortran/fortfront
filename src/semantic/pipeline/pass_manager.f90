module pass_manager
    use base_analyzer, only: base_analyzer_t, analysis_results_t, analyzer_id_t
    use convergence_checker, only: convergence_checker_t
    use analysis_cache, only: analysis_cache_t
    use ast_core, only: ast_arena_t
    use semantic_analyzer, only: semantic_context_t
    use iso_fortran_env, only: error_unit
    implicit none
    private

    public :: pass_manager_t

    type :: analyzer_wrapper_t
        class(base_analyzer_t), allocatable :: analyzer
    end type analyzer_wrapper_t

    type :: pass_manager_t
        type(analyzer_wrapper_t), allocatable :: analyzers(:)
        integer :: num_analyzers = 0
        integer :: max_passes = 10
        logical :: convergence_enabled = .true.
        type(convergence_checker_t) :: checker
        type(analysis_cache_t) :: cache
        integer, allocatable :: execution_order(:)
    contains
        procedure :: add_analyzer => manager_add_analyzer
        procedure :: execute_passes => manager_execute_passes
        procedure :: get_execution_order => manager_get_execution_order
        procedure :: resolve_dependencies => manager_resolve_dependencies
        procedure :: expand_analyzers => manager_expand_analyzers
        procedure :: get_analyzer_call_count => manager_get_analyzer_call_count
    end type pass_manager_t

contains

    subroutine manager_add_analyzer(this, analyzer)
        class(pass_manager_t), intent(inout) :: this
        class(base_analyzer_t), intent(in) :: analyzer

        if (.not. allocated(this%analyzers)) then
            allocate(this%analyzers(10))
        end if

        if (this%num_analyzers >= size(this%analyzers)) then
            call this%expand_analyzers()
        end if

        this%num_analyzers = this%num_analyzers + 1
        allocate(this%analyzers(this%num_analyzers)%analyzer, source=analyzer)
    end subroutine manager_add_analyzer

    subroutine manager_execute_passes(this, ctx, arena, root_index)
        class(pass_manager_t), intent(inout) :: this
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        integer :: pass_num, i
        logical :: converged
        type(analysis_results_t) :: current_results, combined_results

        if (this%num_analyzers == 0) return

        ! Resolve execution order based on dependencies
        call this%resolve_dependencies()
        
        ! Ensure execution order is set
        if (.not. allocated(this%execution_order)) then
            allocate(this%execution_order(this%num_analyzers))
            do i = 1, this%num_analyzers
                this%execution_order(i) = i
            end do
        end if

        ! Reset convergence checker
        call this%checker%reset()

        ! Execute passes
        do pass_num = 1, this%max_passes
            combined_results%converged = .true.
            combined_results%iteration_count = pass_num
            combined_results%changes_made = 0

            ! Execute analyzers in dependency order
            do i = 1, this%num_analyzers
                if (allocated(this%execution_order) .and. &
                    this%execution_order(i) > 0 .and. &
                    this%execution_order(i) <= this%num_analyzers .and. &
                    allocated(this%analyzers(this%execution_order(i))%analyzer)) then
                    current_results = this%analyzers( &
                        this%execution_order(i))%analyzer%analyze(&
                        ctx, arena, root_index)

                    ! Store results in cache
                    call this%cache%store_results(&
                        this%analyzers(this%execution_order(i))%analyzer%id%name, &
                        pass_num, current_results)

                    ! Update combined results
                    combined_results%changes_made = combined_results%changes_made + &
                        current_results%changes_made
                    if (.not. current_results%converged) then
                        combined_results%converged = .false.
                    end if
                else if (.not. allocated(this%execution_order)) then
                    ! Fallback: execute in order added if no dependency resolution
                    if (allocated(this%analyzers(i)%analyzer)) then
                        current_results = this%analyzers(i)%analyzer%analyze(&
                            ctx, arena, root_index)

                        ! Store results in cache
                        call this%cache%store_results(&
                            this%analyzers(i)%analyzer%id%name, &
                            pass_num, current_results)

                        ! Update combined results
                        combined_results%changes_made = &
                            combined_results%changes_made + &
                            current_results%changes_made
                        if (.not. current_results%converged) then
                            combined_results%converged = .false.
                        end if
                    end if
                end if
            end do

            ! Check for convergence if enabled
            if (this%convergence_enabled) then
                ! First check if all individual analyzers have converged
                if (combined_results%converged) then
                    ! All analyzers report converged, we're done
                    converged = .true.
                else
                    ! Not all analyzers converged, continue
                    converged = .false.
                end if
                
                call this%checker%record_state(combined_results)

                if (converged) then
                    exit
                end if
            else
                ! Single pass mode
                exit
            end if
        end do
    end subroutine manager_execute_passes

    function manager_get_execution_order(this) result(order)
        class(pass_manager_t), intent(in) :: this
        integer, allocatable :: order(:)

        if (allocated(this%execution_order)) then
            order = this%execution_order
        else
            allocate(order(0))
        end if
    end function manager_get_execution_order

    subroutine manager_resolve_dependencies(this)
        class(pass_manager_t), intent(inout) :: this
        logical, allocatable :: visited(:), in_stack(:)
        integer :: i, j, dep_id, order_count

        if (this%num_analyzers == 0) return

        if (allocated(this%execution_order)) then
            deallocate(this%execution_order)
        end if
        allocate(this%execution_order(this%num_analyzers))
        allocate(visited(this%num_analyzers))
        allocate(in_stack(this%num_analyzers))

        visited = .false.
        in_stack = .false.
        order_count = 0

        ! Simple topological sort
        do i = 1, this%num_analyzers
            if (.not. visited(i)) then
                call dfs_visit(i)
            end if
        end do

    contains

        recursive subroutine dfs_visit(analyzer_idx)
            integer, intent(in) :: analyzer_idx
            integer :: k, dep_analyzer_idx

            if (in_stack(analyzer_idx)) then
                write(error_unit, '(A)') "ERROR [pass_manager]: Circular dependency detected in analyzers - skipping analysis"
                return  ! Break out of circular dependency
            end if

            if (visited(analyzer_idx)) return

            visited(analyzer_idx) = .true.
            in_stack(analyzer_idx) = .true.

            ! Visit dependencies first
            if (allocated(this%analyzers(analyzer_idx)%analyzer)) then
                if (this%analyzers(analyzer_idx)%analyzer%has_dependencies()) then
                    do k = 1, size(this%analyzers(analyzer_idx)%analyzer%depends_on)
                        dep_id = this%analyzers(analyzer_idx)%analyzer%depends_on(k)
                        
                        ! Find analyzer with this ID
                        dep_analyzer_idx = 0
                        do j = 1, this%num_analyzers
                            if (allocated(this%analyzers(j)%analyzer)) then
                                if (this%analyzers(j)%analyzer%id%id == dep_id) then
                                    dep_analyzer_idx = j
                                    exit
                                end if
                            end if
                        end do

                        if (dep_analyzer_idx > 0) then
                            call dfs_visit(dep_analyzer_idx)
                        end if
                    end do
                end if
            end if

            in_stack(analyzer_idx) = .false.
            order_count = order_count + 1
            this%execution_order(order_count) = analyzer_idx
        end subroutine dfs_visit

    end subroutine manager_resolve_dependencies

    subroutine manager_expand_analyzers(this)
        class(pass_manager_t), intent(inout) :: this
        type(analyzer_wrapper_t), allocatable :: temp(:)
        integer :: old_size

        old_size = size(this%analyzers)
        allocate(temp(old_size * 2))
        temp(1:old_size) = this%analyzers
        deallocate(this%analyzers)
        allocate(this%analyzers(old_size * 2))
        this%analyzers = temp
    end subroutine manager_expand_analyzers

    function manager_get_analyzer_call_count(this, analyzer_name) result(call_count)
        class(pass_manager_t), intent(in) :: this
        character(len=*), intent(in) :: analyzer_name
        integer :: call_count
        integer :: i
        type(analysis_results_t) :: results
        logical :: success
        
        call_count = 0
        
        ! Count how many passes we have cached results for this analyzer
        do i = 1, this%max_passes
            call this%cache%retrieve_results(analyzer_name, i, results, success)
            if (success) then
                call_count = call_count + 1
            else
                exit  ! No more results for higher pass numbers
            end if
        end do
    end function manager_get_analyzer_call_count

end module pass_manager