! Issue #2838: a unary (prefix) user-defined operator must produce an AST whose
! operand is a resolvable arena node, so a backend can walk it like any other
! operator call. The defect emitted the operator node with a dangling operand
! index reported as "argument index does not reference an AST node".
module unary_operator_mod
    implicit none

    interface operator(.negation.)
        module procedure negate
    end interface
contains
    integer function negate(i) result(res)
        integer, intent(in) :: i
        res = -i
    end function negate
end module unary_operator_mod

program issue_2838_unary_defined_operator
    use unary_operator_mod
    implicit none
    integer :: res, x

    x = 10
    res = .negation. x
    print *, res
end program issue_2838_unary_defined_operator
