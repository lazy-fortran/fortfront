! Issue #1611: allocate(type() :: var) must be preserved
program test_allocate_type
    implicit none

    type :: circle
        real :: radius
    end type circle

    type(circle), allocatable :: c
    allocate(circle :: c)
    c%radius = 5.0
end program test_allocate_type
