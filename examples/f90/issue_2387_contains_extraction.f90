program test_contains_extraction
    implicit none
    integer :: x = 42

    x = test_func(x)
    print *, x

contains

    integer function test_func(n)
        implicit none
        integer, intent(in) :: n
        test_func = n * 2
    end function test_func

end program test_contains_extraction