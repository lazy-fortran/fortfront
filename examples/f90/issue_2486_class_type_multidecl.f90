! Test for issue #2486: Multi-variable class/type declarations in procedure bodies
! This file verifies that multi-variable declarations using class and type keywords
! are correctly preserved with all variables.
subroutine test_class_multidecl()
    implicit none
    class(mytype), allocatable :: a(:), b(:)
end subroutine test_class_multidecl

subroutine test_type_multidecl()
    implicit none
    type(mytype), allocatable :: x, y, z
end subroutine test_type_multidecl
