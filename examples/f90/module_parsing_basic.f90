module m
    implicit none
contains
    function add(a,b) result(c)
        integer :: a,b,c
        c = a + b
    end function add
end module m
