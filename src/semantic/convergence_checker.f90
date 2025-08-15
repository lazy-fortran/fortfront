module convergence_checker
    use base_analyzer, only: analysis_results_t
    implicit none
    private

    public :: convergence_checker_t

    type :: convergence_checker_t
        integer :: max_iterations = 100
        real :: tolerance = 1.0e-6
        type(analysis_results_t), allocatable :: previous_state
        integer :: iteration_count = 0
        logical :: enabled = .true.
    contains
        procedure :: record_state => checker_record_state
        procedure :: has_converged => checker_has_converged
        procedure :: reset => checker_reset
        procedure :: set_tolerance => checker_set_tolerance
        procedure :: set_max_iterations => checker_set_max_iterations
    end type convergence_checker_t

contains

    subroutine checker_record_state(this, results)
        class(convergence_checker_t), intent(inout) :: this
        type(analysis_results_t), intent(in) :: results

        if (.not. allocated(this%previous_state)) then
            allocate(this%previous_state)
        end if
        this%previous_state = results
        this%iteration_count = this%iteration_count + 1
    end subroutine checker_record_state

    function checker_has_converged(this, current_results) result(converged)
        class(convergence_checker_t), intent(inout) :: this
        type(analysis_results_t), intent(in) :: current_results
        logical :: converged

        converged = .false.

        if (.not. this%enabled) then
            converged = .false.
            return
        end if

        ! Check for timeout
        if (this%iteration_count >= this%max_iterations) then
            converged = .false.
            return
        end if

        ! If we have no previous state, not converged yet
        if (.not. allocated(this%previous_state)) then
            converged = .false.
            return
        end if

        ! Check if results are within tolerance
        ! For convergence, we care about changes_made and confidence_score stability
        ! iteration_count is expected to increase and converged flag is the result
        if (current_results%changes_made == this%previous_state%changes_made .and. &
            abs(current_results%confidence_score - &
                this%previous_state%confidence_score) < this%tolerance) then
            converged = .true.
        end if
    end function checker_has_converged

    subroutine checker_reset(this)
        class(convergence_checker_t), intent(inout) :: this
        if (allocated(this%previous_state)) then
            deallocate(this%previous_state)
        end if
        this%iteration_count = 0
    end subroutine checker_reset

    subroutine checker_set_tolerance(this, tolerance)
        class(convergence_checker_t), intent(inout) :: this
        real, intent(in) :: tolerance
        this%tolerance = tolerance
    end subroutine checker_set_tolerance

    subroutine checker_set_max_iterations(this, max_iterations)
        class(convergence_checker_t), intent(inout) :: this
        integer, intent(in) :: max_iterations
        this%max_iterations = max_iterations
    end subroutine checker_set_max_iterations

end module convergence_checker