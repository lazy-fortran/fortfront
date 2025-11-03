program main
contains
    subroutine driver()
        call outer()
    end subroutine driver
    subroutine outer()
        call helper()
    contains
        subroutine helper()
            call inner()
        end subroutine helper
        subroutine inner()
        end subroutine inner
    end subroutine outer
end program main
