! Negative fixture for issue #2888 (reject-scope-02), after gfortran.dg.
! The DO construct name repeats the host associated derived type VERTEX that
! the same scoping unit uses in a declaration.
module host_assoc_types_1_mod
    implicit none
    type vertex
        integer :: k
    end type vertex
contains
    subroutine s1()
        type(vertex) :: a
        integer :: i

        vertex: do i = 1, 2
        end do vertex
        a%k = i
    end subroutine s1
end module host_assoc_types_1_mod
