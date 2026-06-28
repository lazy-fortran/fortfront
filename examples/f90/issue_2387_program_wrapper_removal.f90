program test_wrapper
    implicit none
    integer :: result

    result = calculate_value(5)
    print *, "Result:", result
contains
    function calculate_value(x)
        implicit none
        integer, intent(in) :: x
        integer :: calculate_value
        calculate_value = x * 2 + 1
    end function calculate_value
end program test_wrapper
