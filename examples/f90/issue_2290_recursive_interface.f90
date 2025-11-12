module issue_2290_recursive_interface
    implicit none

    interface
        recursive integer function factorial(n) result(res)
            integer, intent(in) :: n
        end function factorial
    end interface

end module issue_2290_recursive_interface
