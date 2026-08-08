module component_storage_rank2
    implicit none

    type :: payload_t
        integer :: value
    end type payload_t

    type :: item_t
        type(payload_t) :: payload
        class(payload_t), allocatable :: owner
    end type item_t

contains

    subroutine exercise(items, i, j)
        type(item_t), intent(inout) :: items(:, :)
        integer, intent(in) :: i, j

        items(i, j)%payload%value = i + j
        items(:, :)%payload%value = 0
        allocate (items(i, j)%owner, source=items(i, j)%payload)
    end subroutine exercise

end module component_storage_rank2
