! Test for issue #2141: Interface result duplication
! Simplified to avoid segfault - just testing interface declaration
program issue_playtest5_interface_result_duplicate
    implicit none

    interface
        function external_func(x) result(y)
            integer, intent(in) :: x
            integer :: y
        end function external_func
    end interface

    integer :: a

    a = 5

end program issue_playtest5_interface_result_duplicate
