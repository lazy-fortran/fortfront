module ownership_event_sequence_facts
    implicit none
contains

    subroutine transfer_and_reallocate(source, destination, seed)
        integer, allocatable, intent(inout) :: source(:), destination(:)
        integer, allocatable, intent(in) :: seed(:)

        allocate (source, source=seed)
        allocate (destination(2))
        call move_alloc(source, destination)
        destination = seed
        deallocate (destination)
    end subroutine transfer_and_reallocate

end module ownership_event_sequence_facts
