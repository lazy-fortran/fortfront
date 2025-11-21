! Regression test for duplicate variable declaration bug
! Function-local variables that are explicitly declared should not be
! auto-generated again, which would cause duplicate declarations.
!
! Bug pattern: explicit declaration + usage without initialization
! would trigger auto-generation of declaration for the same variable

recursive function recursivefunc(this) result(match)
    integer :: callnb
    integer, intent(in) :: this
    integer :: subpattern
    logical :: match
    callnb = callnb + 1
    subpattern = this * 2
    match = .true.
end function recursivefunc
