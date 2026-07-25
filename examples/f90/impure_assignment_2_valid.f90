program impure_assignment_2_valid
    ! VALID neighbour of impure_assignment_2.f90. The PURE function only reads
    ! its dummy argument; the definition of the dummy moves into an impure
    ! subroutine where a variable definition context is allowed.
    implicit none

    type :: node_type
        type(node_type), pointer :: next => null()
    end type node_type

contains

    pure function give_next(node) result(res)
        type(node_type), pointer :: node
        type(node_type), pointer :: res
        res => node%next
    end function give_next

    subroutine link(node, other)
        type(node_type), pointer :: node
        type(node_type), pointer :: other
        node%next => other
    end subroutine link

end program impure_assignment_2_valid
