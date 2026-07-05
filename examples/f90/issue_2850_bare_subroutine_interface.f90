subroutine s
    integer :: x
    interface
        subroutine callee(a)
            integer :: a
        end subroutine
    end interface
    x = 1
    call callee(x)
end subroutine
