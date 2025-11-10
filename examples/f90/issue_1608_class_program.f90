! Issue #1608: CLASS declaration inside program body
program test_class_declaration
    implicit none

    type :: mytype
        integer :: x
    end type mytype

    type :: mytype2
        real :: y
    end type mytype2

    class(mytype), allocatable :: obj
    type(mytype2) :: obj2
end program test_class_declaration
