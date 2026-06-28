! Issue 2737: LFortran generics template and instantiate syntax
template swap(T)
contains
    subroutine swap(a, b)
        type(T), intent(inout) :: a, b
    end subroutine swap
    end template swap

    module m
        instantiate swap{integer}
    contains
        subroutine demo(a, b)
            integer, intent(inout) :: a, b
            call swap{integer}(a, b)
            a = identity{integer}(a)
        end subroutine demo
    end module m
