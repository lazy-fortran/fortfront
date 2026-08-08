module associate_selector_facts
    implicit none

    type :: payload_t
        integer :: value
    end type payload_t

    type :: item_t
        type(payload_t) :: payload
        integer, allocatable :: values(:)
        integer, pointer :: pointer_values(:)
    end type item_t

contains

    subroutine exercise(item, items, value)
        type(item_t), intent(inout) :: item
        type(item_t), intent(inout) :: items(:)
        integer, intent(in) :: value

        associate (component => items(1)%payload, &
                   element => items(1)%values, &
                   pointer => item%pointer_values, &
                   expression => value + 1)
            component%value = value
            element = [value]
            pointer = [value]
            print *, expression
        end associate
    end subroutine exercise

end module associate_selector_facts
