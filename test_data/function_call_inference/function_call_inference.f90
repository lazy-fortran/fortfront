program main
    implicit none
    real(8) :: result

    result = compute(5.0d0)
contains
    real(8) function compute(x)
        implicit none
        real(8), intent(in) :: x
        compute = x*x
    end function compute
end program main
