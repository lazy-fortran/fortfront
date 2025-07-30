program main
    implicit none
    real(8) :: result

    result = add_numbers(5.0d0, 3.0d0)
    print *, result
contains
    real(8) function add_numbers(a, b)
        implicit none
        real(8), intent(in) :: a, b
        add_numbers = a + b
    end function add_numbers
end program main
