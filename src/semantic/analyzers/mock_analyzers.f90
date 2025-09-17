module mock_analyzers
    use base_analyzer, only: base_analyzer_t, analysis_results_t, analyzer_id_t
    use ast_core, only: ast_arena_t
    use semantic_analyzer, only: semantic_context_t
    implicit none
    private

    public :: mock_analyzer_t, convergent_analyzer_t, infinite_analyzer_t, &
              ordered_analyzer_t
    public :: create_test_results, create_stable_results, &
              create_initial_results, create_intermediate_results, &
              create_changing_results, create_precise_results, &
              create_test_results_variant, create_large_test_results, &
              results_equal, get_memory_usage

    ! Mock analyzer that tracks call count
    type, extends(base_analyzer_t) :: mock_analyzer_t
        integer :: call_count = 0
    contains
        procedure :: analyze => mock_analyze
    end type mock_analyzer_t

    ! Analyzer that converges after specified number of passes
    type, extends(base_analyzer_t) :: convergent_analyzer_t
        integer :: call_count = 0
        integer :: convergence_pass = 3
    contains
        procedure :: analyze => convergent_analyze
    end type convergent_analyzer_t

    ! Analyzer that never converges
    type, extends(base_analyzer_t) :: infinite_analyzer_t
        integer :: call_count = 0
    contains
        procedure :: analyze => infinite_analyze
    end type infinite_analyzer_t

    ! Analyzer with ordering capabilities for dependency testing
    type, extends(base_analyzer_t) :: ordered_analyzer_t
        integer :: call_count = 0
        integer :: execution_order = 0
        integer, allocatable :: execution_log(:)
        integer :: log_size = 0
    contains
        procedure :: analyze => ordered_analyze
        procedure :: get_execution_log => get_ordered_execution_log
    end type ordered_analyzer_t

contains

    function mock_analyze(this, ctx, arena, root_index) result(results)
        class(mock_analyzer_t), intent(inout) :: this
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(analysis_results_t) :: results

        this%call_count = this%call_count + 1
        results%converged = .true.
        results%iteration_count = this%call_count
        results%changes_made = 1
        results%status_message = "Mock analysis completed"
        results%confidence_score = 1.0
    end function mock_analyze

    function convergent_analyze(this, ctx, arena, root_index) result(results)
        class(convergent_analyzer_t), intent(inout) :: this
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(analysis_results_t) :: results

        this%call_count = this%call_count + 1
        results%converged = this%call_count >= this%convergence_pass
        results%iteration_count = this%call_count
        results%changes_made = merge(0, 1, results%converged)
        results%status_message = "Convergent analysis"
        results%confidence_score = real(this%call_count) / real(this%convergence_pass)
    end function convergent_analyze

    function infinite_analyze(this, ctx, arena, root_index) result(results)
        class(infinite_analyzer_t), intent(inout) :: this
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(analysis_results_t) :: results

        this%call_count = this%call_count + 1
        results%converged = .false.
        results%iteration_count = this%call_count
        results%changes_made = 1
        results%status_message = "Infinite analysis (never converges)"
        results%confidence_score = 0.5
    end function infinite_analyze

    function ordered_analyze(this, ctx, arena, root_index) result(results)
        class(ordered_analyzer_t), intent(inout) :: this
        type(semantic_context_t), intent(inout) :: ctx
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: root_index
        type(analysis_results_t) :: results

        this%call_count = this%call_count + 1
        
        ! Log execution order
        if (.not. allocated(this%execution_log)) then
            allocate(this%execution_log(100))
            this%log_size = 0
        end if
        
        if (this%log_size < size(this%execution_log)) then
            this%log_size = this%log_size + 1
            this%execution_log(this%log_size) = this%id%id
        end if

        results%converged = .true.
        results%iteration_count = this%call_count
        results%changes_made = 1
        results%status_message = "Ordered analysis"
        results%confidence_score = 1.0
    end function ordered_analyze

    function get_ordered_execution_log(this) result(log)
        class(ordered_analyzer_t), intent(in) :: this
        integer, allocatable :: log(:)

        if (allocated(this%execution_log) .and. this%log_size > 0) then
            allocate(log(this%log_size))
            log = this%execution_log(1:this%log_size)
        else
            allocate(log(0))
        end if
    end function get_ordered_execution_log

    ! Helper functions for test infrastructure

    function create_test_results() result(results)
        type(analysis_results_t) :: results
        results%converged = .true.
        results%iteration_count = 1
        results%changes_made = 5
        results%status_message = "Test results"
        results%confidence_score = 0.95
    end function create_test_results

    function create_stable_results() result(results)
        type(analysis_results_t) :: results
        results%converged = .true.
        results%iteration_count = 1
        results%changes_made = 0
        results%status_message = "Stable results"
        results%confidence_score = 1.0
    end function create_stable_results

    function create_initial_results() result(results)
        type(analysis_results_t) :: results
        results%converged = .false.
        results%iteration_count = 1
        results%changes_made = 10
        results%status_message = "Initial results"
        results%confidence_score = 0.3
    end function create_initial_results

    function create_intermediate_results() result(results)
        type(analysis_results_t) :: results
        results%converged = .false.
        results%iteration_count = 2
        results%changes_made = 5
        results%status_message = "Intermediate results"
        results%confidence_score = 0.7
    end function create_intermediate_results

    function create_changing_results(iteration) result(results)
        integer, intent(in) :: iteration
        type(analysis_results_t) :: results
        results%converged = .false.
        results%iteration_count = iteration
        results%changes_made = iteration
        results%status_message = "Changing results"
        results%confidence_score = 0.5 + real(iteration) * 0.1
    end function create_changing_results

    function create_precise_results(value) result(results)
        real, intent(in) :: value
        type(analysis_results_t) :: results
        results%converged = .true.
        results%iteration_count = 1
        results%changes_made = 0
        results%status_message = "Precise results"
        results%confidence_score = value
    end function create_precise_results

    function create_test_results_variant(variant) result(results)
        integer, intent(in) :: variant
        type(analysis_results_t) :: results
        results%converged = .true.
        results%iteration_count = variant
        results%changes_made = variant * 2
        results%status_message = "Test variant"
        results%confidence_score = 0.8 + real(variant) * 0.05
    end function create_test_results_variant

    function create_large_test_results(size_factor) result(results)
        integer, intent(in) :: size_factor
        type(analysis_results_t) :: results
        results%converged = .true.
        results%iteration_count = size_factor
        results%changes_made = size_factor * 100
        results%status_message = "Large test results"
        results%confidence_score = 0.9
    end function create_large_test_results

    function results_equal(r1, r2) result(equal)
        type(analysis_results_t), intent(in) :: r1, r2
        logical :: equal
        equal = r1 == r2
    end function results_equal

    function get_memory_usage() result(memory_kb)
        integer :: memory_kb
        ! Simplified memory usage - in real implementation would query system
        memory_kb = 1000
    end function get_memory_usage

end module mock_analyzers