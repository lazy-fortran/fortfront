program impure_assignment_2
    ! INVALID: F2008 C1283. In a PURE subprogram a designator whose base object
    ! is a dummy argument of a PURE FUNCTION shall not appear in a variable
    ! definition context, which includes the left side of a pointer assignment.
    implicit none

    type :: node_type
        type(node_type), pointer :: next => null()
    end type node_type

contains

    pure function give_next(node) result(res)
        type(node_type), pointer :: node
        type(node_type), pointer :: res
        res => node%next
        node%next => res
    end function give_next

end program impure_assignment_2
