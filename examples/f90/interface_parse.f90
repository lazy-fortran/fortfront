module iface_mod
    implicit none
    interface
        subroutine do_it(x)
            integer, intent(inout) :: x
        end subroutine do_it
    end interface
end module iface_mod
