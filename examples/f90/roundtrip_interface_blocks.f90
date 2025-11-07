! Comprehensive test for Interface Blocks round-trip
! Tests: interface with result() clause, function statements, subroutine statements,
!        abstract interfaces, generic interfaces

module roundtrip_interface_blocks
    implicit none

    ! Abstract interface
    abstract interface
        real function abstract_func(x)
            real, intent(in) :: x
        end function abstract_func
    end interface

    ! Generic interface with specific procedures
    interface compute
        module procedure compute_scalar
        module procedure compute_array
    end interface compute

contains

    ! Function for generic interface - scalar version
    real function compute_scalar(x) result(res)
        real, intent(in) :: x
        res = x * 2.0
    end function compute_scalar

    ! Function for generic interface - array version
    subroutine compute_array(x, n, res)
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: x
        real, dimension(n), intent(out) :: res
        res = x * 2.0
    end subroutine compute_array

end module roundtrip_interface_blocks

program test_interface_blocks
    use roundtrip_interface_blocks, only: compute
    implicit none

    ! Interface with result() clause
    interface
        function external_with_result(x, y) result(z)
            real, intent(in) :: x, y
            real :: z
        end function external_with_result
    end interface

    ! Interface with function statement (no result clause)
    interface
        real function external_simple(a)
            real, intent(in) :: a
        end function external_simple
    end interface

    ! Interface with subroutine statement
    interface
        subroutine external_sub(input, output, n)
            integer, intent(in) :: n
            real, dimension(n), intent(in) :: input
            real, dimension(n), intent(out) :: output
        end subroutine external_sub
    end interface

    real :: x, y
    real :: arr_in(3), arr_out(3)

    ! Test generic interface from module
    x = 5.0
    y = compute(x)

    arr_in = [1.0, 2.0, 3.0]
    call compute(arr_in, 3, arr_out)

    print *, 'Scalar result:', y
    print *, 'Array result:', arr_out

contains

    ! Provide implementations for the external interfaces
    function external_with_result(x, y) result(z)
        real, intent(in) :: x, y
        real :: z
        z = x + y
    end function external_with_result

    real function external_simple(a)
        real, intent(in) :: a
        external_simple = a * 3.0
    end function external_simple

    subroutine external_sub(input, output, n)
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: input
        real, dimension(n), intent(out) :: output
        output = input + 1.0
    end subroutine external_sub

end program test_interface_blocks
