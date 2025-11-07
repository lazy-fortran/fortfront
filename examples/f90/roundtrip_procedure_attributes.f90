! Comprehensive test for Procedure Attributes round-trip
! Tests: intent(in/out/inout) preservation, elemental keyword, pure keyword,
!        recursive keyword, result() clause

module roundtrip_procedure_attributes
    implicit none

contains

    ! Pure function - no side effects
    pure function pure_add(a, b) result(c)
        real, intent(in) :: a, b
        real :: c
        c = a + b
    end function pure_add

    ! Elemental function - operates element-wise on arrays
    elemental function square(x) result(x2)
        real, intent(in) :: x
        real :: x2
        x2 = x * x
    end function square

    ! Pure elemental function
    pure elemental function cube(x) result(x3)
        real, intent(in) :: x
        real :: x3
        x3 = x * x * x
    end function cube

    ! Recursive function - factorial
    recursive function factorial(n) result(fact)
        integer, intent(in) :: n
        integer :: fact

        if (n <= 1) then
            fact = 1
        else
            fact = n * factorial(n - 1)
        end if
    end function factorial

    ! Subroutine with intent(in) parameters
    subroutine read_only(input, n)
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: input
        print *, 'Sum:', sum(input)
    end subroutine read_only

    ! Subroutine with intent(out) parameters
    subroutine write_only(output, n, value)
        integer, intent(in) :: n
        real, intent(in) :: value
        real, dimension(n), intent(out) :: output
        output = value
    end subroutine write_only

    ! Subroutine with intent(inout) parameters
    subroutine modify(data, n, factor)
        integer, intent(in) :: n
        real, intent(in) :: factor
        real, dimension(n), intent(inout) :: data
        data = data * factor
    end subroutine modify

    ! Function with result() clause and multiple operations
    function complex_calculation(x, y, z) result(output)
        real, intent(in) :: x, y, z
        real :: output
        real :: temp

        temp = x * y
        output = temp + z
    end function complex_calculation

    ! Recursive subroutine - print countdown
    recursive subroutine countdown(n)
        integer, intent(in) :: n

        if (n > 0) then
            print *, n
            call countdown(n - 1)
        end if
    end subroutine countdown

end module roundtrip_procedure_attributes

program test_procedure_attributes
    use roundtrip_procedure_attributes
    implicit none

    real :: a, b, c
    real :: arr(5)
    integer :: i, fact_val

    ! Test pure function
    a = 3.0
    b = 4.0
    c = pure_add(a, b)

    ! Test elemental functions on arrays
    arr = [1.0, 2.0, 3.0, 4.0, 5.0]
    arr = square(arr)  ! Element-wise squaring

    ! Test recursive function
    fact_val = factorial(5)

    ! Test intent variations
    call read_only(arr, 5)
    call write_only(arr, 5, 10.0)
    call modify(arr, 5, 2.0)

    ! Test result() clause
    c = complex_calculation(2.0, 3.0, 4.0)

    print *, 'Pure add result:', c
    print *, 'Modified array:', arr(1:3)
    print *, 'Factorial(5):', fact_val

end program test_procedure_attributes
