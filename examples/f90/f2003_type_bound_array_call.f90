! Example demonstrating type-bound procedure calls with array indices
! ISO/IEC 1539-1:2018 Section 15.5.1 - Type-bound procedure references
module type_bound_array_call_mod
    implicit none

    type :: shape_t
        integer :: id
        real :: area
    contains
        procedure :: compute => compute_shape
        procedure :: display => display_shape
    end type shape_t

contains

    subroutine compute_shape(this, scale_factor)
        class(shape_t), intent(inout) :: this
        real, intent(in) :: scale_factor
        this%area = this%area * scale_factor
    end subroutine compute_shape

    subroutine display_shape(this)
        class(shape_t), intent(in) :: this
        print *, 'Shape ID:', this%id, 'Area:', this%area
    end subroutine display_shape

    subroutine process_shapes(shapes, n)
        integer, intent(in) :: n
        type(shape_t), intent(inout) :: shapes(n)
        integer :: i

        ! Type-bound procedure calls with array indices
        do i = 1, n
            call shapes(i)%compute(2.0)
            call shapes(i)%display()
        end do

    end subroutine process_shapes

end module type_bound_array_call_mod
