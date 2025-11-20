program test_interface
    implicit none
    integer :: value

    interface
        subroutine setup_value(x)
            integer, intent(inout) :: x
        end subroutine setup_value
    end interface

    value = 3
    call setup_value(value)
    print *, value
end program test_interface

subroutine setup_value(x)
    integer, intent(inout) :: x
    x = x + 1
end subroutine setup_value
