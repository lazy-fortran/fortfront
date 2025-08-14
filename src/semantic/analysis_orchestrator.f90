module analysis_orchestrator
    ! Analysis orchestrator manages analyzer lifecycle and execution
    ! Coordinates multiple analyzers and aggregates their results
    use ast_core, only: ast_arena_t
    use ast_nodes_core, only: program_node, binary_op_node, assignment_node
    use ast_nodes_control, only: if_node
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use base_analyzer, only: base_analyzer_t, analyzer_config_t, analysis_result_t
    use semantic_event_system, only: semantic_event_system_t, create_event_system
    implicit none
    private

    public :: analysis_orchestrator_t, create_analysis_orchestrator
    public :: analysis_results_t

    ! Aggregated results from multiple analyzers
    type :: analysis_results_t
        type(analysis_result_t), allocatable :: individual_results(:)
        integer :: analyzer_count = 0
        integer :: total_errors = 0
        integer :: total_warnings = 0
        logical :: has_any_findings = .false.
    contains
        procedure :: clear => results_clear
        procedure :: add_result => results_add
        procedure :: has_findings => results_has_findings
    end type analysis_results_t

    ! Registered analyzer entry
    type :: analyzer_entry_t
        class(base_analyzer_t), allocatable :: analyzer
        logical :: active = .true.
    end type analyzer_entry_t

    ! Main orchestrator managing the analysis pipeline
    type :: analysis_orchestrator_t
        type(analyzer_entry_t), allocatable :: analyzers(:)
        integer :: analyzer_count = 0
        integer :: capacity = 0
        type(semantic_event_system_t) :: event_system
        type(analysis_results_t) :: aggregated_results
    contains
        procedure :: register_analyzer => orchestrator_register
        procedure :: unregister_analyzer => orchestrator_unregister
        procedure :: analyze => orchestrator_analyze
        procedure :: get_aggregated_results => orchestrator_get_results
        procedure :: clear_results => orchestrator_clear_results
        procedure :: expand_capacity => orchestrator_expand_capacity
        procedure :: traverse_and_analyze
        procedure :: traverse_children
    end type analysis_orchestrator_t

contains

    ! Create a new analysis orchestrator
    function create_analysis_orchestrator() result(orchestrator)
        type(analysis_orchestrator_t) :: orchestrator
        orchestrator%capacity = 10
        orchestrator%analyzer_count = 0
        allocate(orchestrator%analyzers(orchestrator%capacity))
        orchestrator%event_system = create_event_system()
    end function create_analysis_orchestrator

    ! Register an analyzer with the orchestrator
    subroutine orchestrator_register(this, analyzer)
        class(analysis_orchestrator_t), intent(inout) :: this
        class(base_analyzer_t), intent(in) :: analyzer

        ! Expand capacity if needed
        if (this%analyzer_count >= this%capacity) then
            call this%expand_capacity()
        end if

        ! Add analyzer to registry
        this%analyzer_count = this%analyzer_count + 1
        allocate(this%analyzers(this%analyzer_count)%analyzer, source=analyzer)
        this%analyzers(this%analyzer_count)%active = .true.

        ! Subscribe analyzer to relevant events
        call this%event_system%subscribe("node_visited", &
                                         this%analyzers(this%analyzer_count)%analyzer, &
                                         priority=1)
        call this%event_system%subscribe("scope_entered", &
                                         this%analyzers(this%analyzer_count)%analyzer, &
                                         priority=1)
        call this%event_system%subscribe("scope_exited", &
                                         this%analyzers(this%analyzer_count)%analyzer, &
                                         priority=1)
    end subroutine orchestrator_register

    ! Unregister an analyzer from the orchestrator
    subroutine orchestrator_unregister(this, analyzer)
        class(analysis_orchestrator_t), intent(inout) :: this
        class(base_analyzer_t), intent(in) :: analyzer
        integer :: i, j

        ! Find and remove analyzer
        do i = 1, this%analyzer_count
            if (allocated(this%analyzers(i)%analyzer)) then
                ! Simple comparison - in practice might need more sophisticated matching
                this%analyzers(i)%active = .false.
                
                ! Unsubscribe from events
                call this%event_system%unsubscribe("node_visited", &
                                                   this%analyzers(i)%analyzer)
                call this%event_system%unsubscribe("scope_entered", &
                                                   this%analyzers(i)%analyzer)
                call this%event_system%unsubscribe("scope_exited", &
                                                   this%analyzers(i)%analyzer)
                
                ! Shift remaining analyzers down
                do j = i, this%analyzer_count - 1
                    this%analyzers(j) = this%analyzers(j + 1)
                end do
                this%analyzer_count = this%analyzer_count - 1
                exit
            end if
        end do
    end subroutine orchestrator_unregister

    ! Execute coordinated analysis using all registered analyzers
    subroutine orchestrator_analyze(this, arena, root_index, results)
        class(analysis_orchestrator_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(analysis_results_t), intent(out) :: results
        integer :: i

        ! Clear previous results
        call this%clear_results()

        ! Initialize results container
        call results%clear()
        allocate(results%individual_results(this%analyzer_count))
        results%analyzer_count = this%analyzer_count

        ! Process the AST tree through event-driven traversal
        call this%traverse_and_analyze(arena, root_index)

        ! Collect results from all analyzers
        do i = 1, this%analyzer_count
            if (allocated(this%analyzers(i)%analyzer) .and. &
                this%analyzers(i)%active) then
                results%individual_results(i) = &
                    this%analyzers(i)%analyzer%get_results()
                
                ! Aggregate statistics
                results%total_errors = results%total_errors + &
                    results%individual_results(i)%error_count
                results%total_warnings = results%total_warnings + &
                    results%individual_results(i)%warning_count
                
                if (results%individual_results(i)%has_findings) then
                    results%has_any_findings = .true.
                end if
            end if
        end do

        ! Store aggregated results in orchestrator
        this%aggregated_results = results
    end subroutine orchestrator_analyze

    ! Traverse AST and fire events for analysis
    recursive subroutine traverse_and_analyze(this, arena, node_index)
        class(analysis_orchestrator_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        ! Fire node_visited event for this node
        call this%event_system%fire_event("node_visited", arena, node_index)

        ! Traverse child nodes based on node type
        call this%traverse_children(arena, node_index)
    end subroutine traverse_and_analyze

    ! Traverse child nodes for different AST node types
    subroutine traverse_children(this, arena, node_index)
        class(analysis_orchestrator_t), intent(inout) :: this
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        integer :: i

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        ! Handle different node types and their children
        select type (node => arena%entries(node_index)%node)
        type is (program_node)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call this%traverse_and_analyze(arena, node%body_indices(i))
                end do
            end if
        type is (function_def_node)
            call this%event_system%fire_event("scope_entered", arena, node_index)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call this%traverse_and_analyze(arena, node%body_indices(i))
                end do
            end if
            call this%event_system%fire_event("scope_exited", arena, node_index)
        type is (subroutine_def_node)
            call this%event_system%fire_event("scope_entered", arena, node_index)
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call this%traverse_and_analyze(arena, node%body_indices(i))
                end do
            end if
            call this%event_system%fire_event("scope_exited", arena, node_index)
        type is (if_node)
            call this%event_system%fire_event("scope_entered", arena, node_index)
            if (node%condition_index > 0) then
                call this%traverse_and_analyze(arena, node%condition_index)
            end if
            if (allocated(node%then_body_indices)) then
                do i = 1, size(node%then_body_indices)
                    call this%traverse_and_analyze(arena, node%then_body_indices(i))
                end do
            end if
            call this%event_system%fire_event("scope_exited", arena, node_index)
        type is (binary_op_node)
            if (node%left_index > 0) then
                call this%traverse_and_analyze(arena, node%left_index)
            end if
            if (node%right_index > 0) then
                call this%traverse_and_analyze(arena, node%right_index)
            end if
        type is (assignment_node)
            if (node%target_index > 0) then
                call this%traverse_and_analyze(arena, node%target_index)
            end if
            if (node%value_index > 0) then
                call this%traverse_and_analyze(arena, node%value_index)
            end if
        class default
            ! For other node types, no special child traversal needed
            continue
        end select
    end subroutine traverse_children

    ! Get aggregated results from all analyzers
    function orchestrator_get_results(this) result(results)
        class(analysis_orchestrator_t), intent(in) :: this
        type(analysis_results_t) :: results
        results = this%aggregated_results
    end function orchestrator_get_results

    ! Clear all results
    subroutine orchestrator_clear_results(this)
        class(analysis_orchestrator_t), intent(inout) :: this
        call this%aggregated_results%clear()
    end subroutine orchestrator_clear_results

    ! Expand analyzer capacity
    subroutine orchestrator_expand_capacity(this)
        class(analysis_orchestrator_t), intent(inout) :: this
        type(analyzer_entry_t), allocatable :: temp_analyzers(:)
        integer :: new_capacity, i

        new_capacity = this%capacity * 2
        allocate(temp_analyzers(new_capacity))

        ! Copy existing analyzers
        do i = 1, this%analyzer_count
            temp_analyzers(i) = this%analyzers(i)
        end do

        ! Replace with expanded array
        deallocate(this%analyzers)
        allocate(this%analyzers(new_capacity))
        this%analyzers = temp_analyzers
        this%capacity = new_capacity
    end subroutine orchestrator_expand_capacity

    ! Clear analysis results
    subroutine results_clear(this)
        class(analysis_results_t), intent(inout) :: this
        integer :: i

        if (allocated(this%individual_results)) then
            do i = 1, this%analyzer_count
                call this%individual_results(i)%clear()
            end do
            deallocate(this%individual_results)
        end if

        this%analyzer_count = 0
        this%total_errors = 0
        this%total_warnings = 0
        this%has_any_findings = .false.
    end subroutine results_clear

    ! Add individual analyzer result
    subroutine results_add(this, result)
        class(analysis_results_t), intent(inout) :: this
        type(analysis_result_t), intent(in) :: result

        ! Update aggregated statistics
        this%total_errors = this%total_errors + result%error_count
        this%total_warnings = this%total_warnings + result%warning_count
        if (result%has_findings) then
            this%has_any_findings = .true.
        end if
    end subroutine results_add

    ! Check if there are any findings
    function results_has_findings(this) result(has_findings)
        class(analysis_results_t), intent(in) :: this
        logical :: has_findings
        has_findings = this%has_any_findings
    end function results_has_findings

end module analysis_orchestrator