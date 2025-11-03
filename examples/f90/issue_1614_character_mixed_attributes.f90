program test_case_5
    implicit none
    integer, parameter :: n = 10
    character(len=:), allocatable :: str1
    character(len=n), dimension(5) :: str_array
    character(len=*), parameter :: greeting = "Hello"
    str1 = "Test"
    str_array(1) = "Item"
end program test_case_5
