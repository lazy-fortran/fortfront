program test_parameter_statement
    implicit none
    integer :: x
    character(len=*) :: MY_STRING(1:3)
    parameter(x=5)
    parameter(MY_STRING=(/"AC ", "B  ", "C  "/))
    print *, x, MY_STRING
end program test_parameter_statement
