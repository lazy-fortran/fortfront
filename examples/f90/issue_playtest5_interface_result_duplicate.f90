program issue_playtest5_interface_result_duplicate
    implicit none

    real :: x, y

    interface
        function external_func(x) result(y)
            real, intent(in) :: x
            real :: y
        end function external_func
    end interface

    x = 5.0
    y = external_func(x)
    print *, 'Result:', y

end program issue_playtest5_interface_result_duplicate
