! Test nested implicit-do array constructor
program test_nested_array_constructor
    implicit none
    integer :: i, j
    integer, parameter :: n = size((/((/(i * j, i=1, j)/), j=1, 2)/))

    print *, "Array size:", n
end program test_nested_array_constructor
