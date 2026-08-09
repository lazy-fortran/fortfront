program polymorphic_assignment_replay_facts
    implicit none

    type :: payload_t
        integer, allocatable :: values(:)
    end type payload_t

    type, extends(payload_t) :: child_t
        integer :: tag
    end type child_t

    type :: holder_t
        class(payload_t), allocatable :: item
    end type holder_t

    type(holder_t) :: box
    type(holder_t), save :: shared
    type(holder_t), target :: alias_box
    class(payload_t), allocatable :: unknown
    type(child_t) :: rhs
    logical :: execute_boundaries

    allocate (rhs%values(2))
    rhs%values = [2, 3]
    rhs%tag = 7

    ! Intrinsic assignment allocates the polymorphic component with the
    ! concrete source dynamic type and deep-copies its allocatable payload.
    box%item = rhs
    if (.not. allocated(box%item)) error stop 1

    select type (stored => box%item)
        type is (child_t)
            if (stored%tag /= 7) error stop 2
            if (.not. allocated(stored%values)) error stop 3
            if (sum(stored%values) /= 5) error stop 4
            rhs%values = [9, 9]
            if (sum(stored%values) /= 5) error stop 5
        class default
            error stop 6
    end select

    ! Keep the refusal-only internal procedures in the compiled fixture so
    ! the API test can inspect their ASTs without executing unsafe cases.
    execute_boundaries = .false.
    if (execute_boundaries) then
        call polymorphic_source_boundary(unknown)
        call target_destination_boundary(alias_box)
        call global_destination_boundary()
        call control_flow_boundary(execute_boundaries)
    end if

contains

    subroutine polymorphic_source_boundary(unknown)
        class(payload_t), allocatable, intent(in) :: unknown

        box%item = unknown
    end subroutine polymorphic_source_boundary

    subroutine target_destination_boundary(alias_box)
        type(holder_t), target, intent(inout) :: alias_box

        alias_box%item = rhs
    end subroutine target_destination_boundary

    subroutine global_destination_boundary()
        shared%item = rhs
    end subroutine global_destination_boundary

    subroutine control_flow_boundary(flag)
        logical, intent(in) :: flag

        if (flag) box%item = rhs
    end subroutine control_flow_boundary

end program polymorphic_assignment_replay_facts
