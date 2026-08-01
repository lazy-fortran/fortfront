! Corrected neighbour of host_assoc_types_1.f90 (issue #2888).
! A construct name that differs from the host associated type name is legal.
module host_assoc_types_1_corrected_mod
    implicit none
    type vertex
        integer :: k
    end type vertex
contains
    subroutine s1()
        type(vertex) :: a
        integer :: i

        scan_vertices: do i = 1, 2
        end do scan_vertices
        a%k = i
    end subroutine s1
end module host_assoc_types_1_corrected_mod
