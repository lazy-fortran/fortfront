! Three-level derived type inheritance chain (issue #2819).
! Exercises the type_hierarchy tracker: shape_t -> polygon_t -> square_t.
program type_hierarchy_chain
    implicit none

    type :: shape_t
        real :: area
    end type shape_t

    type, extends(shape_t) :: polygon_t
        integer :: sides
    end type polygon_t

    type, extends(polygon_t) :: square_t
        real :: side_length
    end type square_t

    type(square_t) :: sq

    sq%side_length = 3.0
    sq%sides = 4
    sq%area = sq%side_length*sq%side_length
    print *, sq%area
end program type_hierarchy_chain
