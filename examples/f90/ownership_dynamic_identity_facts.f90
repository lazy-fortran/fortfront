module ownership_dynamic_identity_facts
    implicit none

    type, abstract :: base_t
        integer :: value
    end type base_t

    type, extends(base_t) :: child_t
        integer :: child_value
    end type child_t

    class(base_t), allocatable :: shared(:)

contains

    subroutine transfer_identity(source, destination)
        class(base_t), allocatable, intent(inout) :: source(:), destination(:)
        type(child_t), allocatable :: seed(:), replacement(:)
        type(child_t), allocatable, target :: aliased(:)

        allocate (seed(2))
        call move_alloc(seed, source)
        call move_alloc(source, destination)
        allocate (replacement(3))
        destination = replacement
        allocate (aliased(1))
        call move_alloc(aliased, destination)
        allocate (shared(1), source=replacement)
        call move_alloc(shared, destination)
        deallocate (destination)
    end subroutine transfer_identity

end module ownership_dynamic_identity_facts
