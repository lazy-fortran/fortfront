program expression_queries
    implicit none

    type :: box_t
        integer :: value
    end type box_t

    integer, target :: values(5)
    integer, target :: scalar_target
    integer, pointer :: ptr
    integer :: selected(3)
    integer :: bounded(2:6)
    type(box_t) :: box

    selected = [integer :: 1, 2, 3]
    selected = values(1:5:2)
    box%value = selected(1)
    ptr => scalar_target
    nullify (ptr)
contains
    subroutine consume(items)
        integer :: items(*)
    end subroutine consume
end program expression_queries
