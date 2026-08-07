module component_storage_query
    implicit none

    type :: base_t
    end type base_t

    type :: holder_t
        class(base_t), allocatable :: owner
        class(*), allocatable :: payload
    end type holder_t
contains

    subroutine allocate_components(box)
        type(holder_t), intent(inout) :: box
        allocate (box%owner)
        allocate (box%payload)
    end subroutine allocate_components

end module component_storage_query
