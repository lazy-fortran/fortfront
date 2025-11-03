! Issue #1616: polymorphic allocatable/pointer arrays must keep their declarations
module polymorphic_arrays
    implicit none

    type :: Shape
        real :: area
    end type Shape

    type, extends(Shape) :: Circle
        real :: radius
    end type Circle

    type :: List_circle
        type(Circle), allocatable :: alloc_type(:)
        class(Circle), allocatable :: alloc_class(:)
        class(Circle), pointer :: ptr_class(:)
    end type List_circle

    type :: Container
        type(Circle), allocatable :: regular(:)
        class(Shape), allocatable :: polymorphic(:)
    end type Container
end module polymorphic_arrays
