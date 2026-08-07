module component_storage_query
    implicit none

    type :: base_t
    end type base_t

    type :: holder_t
        class(base_t), allocatable :: owner
        class(*), allocatable :: payload
    end type holder_t

    type :: container_t
        class(base_t), allocatable :: owner
        class(*), allocatable :: payload
        type(holder_t) :: nested
    end type container_t
contains

    subroutine allocate_components(boxes)
        type(container_t), intent(inout) :: boxes(:)
        allocate (boxes(1)%owner)
        allocate (boxes(1)%payload)
        allocate (boxes(1)%nested%owner)
        allocate (boxes(1)%nested%payload)
    end subroutine allocate_components

end module component_storage_query
