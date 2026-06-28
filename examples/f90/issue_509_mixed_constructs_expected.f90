module m
contains
    subroutine foo()
        print *, "foo"
    end subroutine foo
    subroutine bar()
        print *, "bar"
    end subroutine bar
    function twice(x)
        y = 2*x
    end function twice
    function thrice(x)
        y = 3*x
    end function thrice
end module m
