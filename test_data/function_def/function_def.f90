program main
    implicit none
contains
    real(8) function compute(x)
        implicit none
        real(8), intent(in) :: x
        compute = x*x
    end function compute
end program main
