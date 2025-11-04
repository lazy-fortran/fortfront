program test_include_statement
    implicit none
    integer :: x

    x = 42
    include '/tmp/include_test.f90'
    print *, "x =", x
end program test_include_statement
