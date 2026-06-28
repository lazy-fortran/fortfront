program test
    real function f(x)
        real :: x
        f = x * 2
    end function
    subroutine sub()
        print *, 'hello'
    end subroutine
end program
