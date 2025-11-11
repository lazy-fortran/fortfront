module demo_pure_interface
    implicit none

    ! Interface with pure subroutine
    interface
        pure subroutine touch(x)
            integer, intent(in) :: x
        end subroutine touch
    end interface

    ! Interface with elemental function
    interface
        elemental function square(x)
            real, intent(in) :: x
            real :: square
        end function square
    end interface

    ! Interface with pure elemental function
    interface
        pure elemental function add(x, y)
            integer, intent(in) :: x, y
            integer :: add
        end function add
    end interface

    ! Interface with recursive function
    interface
        recursive function factorial(n) result(res)
            integer, intent(in) :: n
            integer :: res
        end function factorial
    end interface

end module demo_pure_interface
