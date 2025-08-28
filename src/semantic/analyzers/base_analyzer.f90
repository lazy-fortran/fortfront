module base_analyzer
    use ast_core, only: ast_arena_t
    use semantic_analyzer, only: semantic_context_t
    implicit none
    private

    public :: base_analyzer_t, analysis_results_t, analyzer_id_t

    ! Type for analyzer identification
    type :: analyzer_id_t
        integer :: id = 0
        character(len=32) :: name = ""
    end type analyzer_id_t

    ! Type for analysis results
    type :: analysis_results_t
        logical :: converged = .false.
        integer :: iteration_count = 0
        integer :: changes_made = 0
        character(len=256) :: status_message = ""
        real :: confidence_score = 1.0
    contains
        procedure :: equals => results_equals
        procedure :: assign => results_assign
        generic :: operator(==) => equals
        generic :: assignment(=) => assign
    end type analysis_results_t

    ! Abstract base analyzer type
    type, abstract :: base_analyzer_t
        type(analyzer_id_t) :: id
        logical :: enabled = .true.
        integer :: priority = 0
        integer, allocatable :: depends_on(:)
    contains
        procedure(analyze_interface), deferred :: analyze
        procedure :: get_id => analyzer_get_id
        procedure :: set_dependencies => analyzer_set_dependencies
        procedure :: has_dependencies => analyzer_has_dependencies
    end type base_analyzer_t

    abstract interface
        function analyze_interface(this, ctx, arena, root_index) result(results)
            import :: base_analyzer_t, semantic_context_t, ast_arena_t, &
                      analysis_results_t
            class(base_analyzer_t), intent(inout) :: this
            type(semantic_context_t), intent(inout) :: ctx
            type(ast_arena_t), intent(inout) :: arena
            integer, intent(in) :: root_index
            type(analysis_results_t) :: results
        end function analyze_interface
    end interface

contains

    function results_equals(this, other) result(equal)
        class(analysis_results_t), intent(in) :: this, other
        logical :: equal
        real, parameter :: tolerance = 1.0e-6

        equal = this%converged .eqv. other%converged .and. &
                this%iteration_count == other%iteration_count .and. &
                this%changes_made == other%changes_made .and. &
                abs(this%confidence_score - other%confidence_score) < tolerance
    end function results_equals

    subroutine results_assign(lhs, rhs)
        class(analysis_results_t), intent(out) :: lhs
        type(analysis_results_t), intent(in) :: rhs

        lhs%converged = rhs%converged
        lhs%iteration_count = rhs%iteration_count
        lhs%changes_made = rhs%changes_made
        lhs%status_message = rhs%status_message
        lhs%confidence_score = rhs%confidence_score
    end subroutine results_assign

    function analyzer_get_id(this) result(analyzer_id)
        class(base_analyzer_t), intent(in) :: this
        type(analyzer_id_t) :: analyzer_id
        analyzer_id = this%id
    end function analyzer_get_id

    subroutine analyzer_set_dependencies(this, dependency_ids)
        class(base_analyzer_t), intent(inout) :: this
        integer, intent(in) :: dependency_ids(:)
        this%depends_on = dependency_ids
    end subroutine analyzer_set_dependencies

    function analyzer_has_dependencies(this) result(has_deps)
        class(base_analyzer_t), intent(in) :: this
        logical :: has_deps
        has_deps = allocated(this%depends_on) .and. size(this%depends_on) > 0
    end function analyzer_has_dependencies

end module base_analyzer