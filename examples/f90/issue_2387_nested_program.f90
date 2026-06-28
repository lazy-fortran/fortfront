program main_program
    implicit none

    ! Main program variables
    integer :: a, b, c

    ! Simple calculations
    a = 10
    b = 20
    c = a + b

    ! Call helper function
    print *, "Sum:", c
    print *, "Squared:", square_number(c)

contains
    ! Helper function
    integer function square_number(x)
        implicit none
        integer, intent(in) :: x
        square_number = x * x
    end function square_number

    ! Another helper function
    integer function add_five(x)
        implicit none
        integer, intent(in) :: x
        add_five = x + 5
    end function add_five
end program main_program
