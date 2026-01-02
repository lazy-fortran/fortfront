! Reproducer for issue #1738: derived type with allocatable component
program test_derived_type_alloc
    implicit none

    type :: vector_t
        integer :: size
        real, allocatable :: values(:)
    end type vector_t

    type(vector_t) :: v
    integer :: i

    v%size = 3
    allocate (v%values(v%size))

    do i = 1, v%size
        v%values(i) = real(i) * 1.5
    end do

    print *, "Vector size:", v%size
    print *, "Vector values:", v%values

    deallocate (v%values)
end program test_derived_type_alloc
