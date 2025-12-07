! Example demonstrating abstract interface in module and procedure scope
! ISO/IEC 1539-1:2018 Section 15.4.3.2 - Abstract interface blocks
module abstract_interface_demo_mod
    implicit none

    ! Module-level abstract interface
    abstract interface
        function transform_func(x) result(y)
            real, intent(in) :: x
            real :: y
        end function transform_func
    end interface

    ! Module-level procedure pointer using abstract interface
    procedure(transform_func), pointer :: module_func_ptr => null()

contains

    subroutine apply_transform(x, result_val)
        real, intent(in) :: x
        real, intent(out) :: result_val

        ! Procedure-level abstract interface
        abstract interface
            function unary_func(val) result(res)
                real, intent(in) :: val
                real :: res
            end function unary_func
        end interface

        procedure(unary_func), pointer :: local_op

        local_op => null()
        result_val = x * 2.0
    end subroutine apply_transform

end module abstract_interface_demo_mod
