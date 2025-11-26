! Test case for issue #2489: interface blocks inside procedure bodies
! Interface blocks should be preserved when parsed inside function/subroutine bodies
module test_interface_in_procedure_mod
    implicit none
contains
    subroutine call_external_sub()
        implicit none
        interface
            subroutine ext_sub(x)
                integer, intent(in) :: x
            end subroutine ext_sub
        end interface
        call ext_sub(42)
    end subroutine call_external_sub

    function call_external_func() result(res)
        implicit none
        integer :: res
        interface
            function ext_func(y) result(r)
                integer, intent(in) :: y
                integer :: r
            end function ext_func
        end interface
        res = ext_func(10)
    end function call_external_func
end module test_interface_in_procedure_mod
