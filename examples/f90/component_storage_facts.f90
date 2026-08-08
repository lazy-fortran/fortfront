module component_storage_facts
    implicit none

    type :: base_t
        integer :: value
    end type base_t

    type :: item_t
        type(base_t) :: concrete
        integer, allocatable :: values(:)
        integer, pointer :: pointer_values(:)
        class(base_t), allocatable :: polymorphic
    end type item_t
contains

    subroutine exercise(item, items, ordinary)
        type(item_t), intent(inout) :: item
        type(item_t), intent(inout) :: items(:)
        integer, intent(in) :: ordinary

        item%concrete%value = ordinary
        item%values = [ordinary, ordinary + 1]
        items(1)%concrete%value = ordinary
        allocate (items(1)%polymorphic, source=item%concrete)
        item%pointer_values => item%values

        associate (alias => item%concrete)
            alias%value = ordinary
        end associate
    end subroutine exercise

end module component_storage_facts
