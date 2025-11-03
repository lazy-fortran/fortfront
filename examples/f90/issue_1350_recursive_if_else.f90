module recursion_mod
    implicit none
contains

    recursive function factorial(n) result(f)
        integer, intent(in) :: n
        integer :: f

        if (n <= 1) then
            f = 1
        else
            f = n * factorial(n - 1)
        end if
    end function factorial

end module recursion_mod
