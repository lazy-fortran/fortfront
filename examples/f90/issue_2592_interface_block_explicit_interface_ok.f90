program issue_2592_interface_block_explicit_interface_ok
    implicit none

    integer :: x

    interface
        subroutine external_sub(a)
            implicit none

            integer, intent(in) :: a
        end subroutine external_sub
    end interface

    x = 1
    call external_sub(x)
end program issue_2592_interface_block_explicit_interface_ok

