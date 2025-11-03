! Issue #1608: CLASS declaration inside program body
program test_class_declaration
    implicit none

    class(mytype) :: obj
    type(mytype2) :: obj2
end program test_class_declaration
