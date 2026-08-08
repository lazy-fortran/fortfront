module ownership_reallocation_facts
    implicit none

    type :: holder_t
        integer, allocatable :: values(:)
        integer :: ordinary
    end type holder_t
contains

    subroutine assign_values(box, rhs, n)
        type(holder_t), intent(inout) :: box
        integer, allocatable, intent(in) :: rhs(:)
        integer, intent(in) :: n
        integer, allocatable :: temporary(:)

        allocate (temporary(n))
        allocate (box%values, source=rhs)
        box%values = rhs
        box%values(1) = rhs(1)
        box%ordinary = rhs(1)
    end subroutine assign_values

end module ownership_reallocation_facts
