program main
    implicit none
    integer :: x, y, result

    x = 5
    y = 3
    result = add_numbers(x, y)
    print *, "Result:", result

contains
    function add_numbers(a, b)
        implicit none
        integer, intent(in) :: a, b
        integer :: add_numbers

        add_numbers = a + b
    end function add_numbers
end program main
