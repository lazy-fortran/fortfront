program impure_assignment_2_valid
    ! VALID neighbour of impure_assignment_2.f90. The pointer result is
    ! produced by a PURE subroutine, where a POINTER dummy may be the target
    ! of a pointer assignment; the definition of the dummy moves into an impure
    ! subroutine where a variable definition context is allowed.
    implicit none

    type :: node_type
        type(node_type), pointer :: next => null()
    end type node_type

contains

    pure subroutine give_next(node, res)
        type(node_type), pointer :: node
        type(node_type), pointer :: res
        res => node%next
    end subroutine give_next

    subroutine link(node, other)
        type(node_type), pointer :: node
        type(node_type), pointer :: other
        node%next => other
    end subroutine link

end program impure_assignment_2_valid
