module pure_elemental_demo
    implicit none
contains
    pure function square(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x * x
    end function square

    elemental function add_one(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x + 1
    end function add_one

    pure subroutine compute_sum(a, b, c)
        integer, intent(in) :: a, b
        integer, intent(out) :: c
        c = a + b
    end subroutine compute_sum

    elemental subroutine double_value(x)
        integer, intent(inout) :: x
        x = x * 2
    end subroutine double_value

    recursive pure function factorial(n) result(f)
        integer, intent(in) :: n
        integer :: f
        if (n <= 1) then
            f = 1
        else
            f = n * factorial(n - 1)
        end if
    end function factorial

end module pure_elemental_demo

program test_pure_elemental
    use pure_elemental_demo, only: square, add_one, compute_sum, double_value, &
        factorial
    implicit none
    integer :: arr(3), result_arr(3), val

    arr = [1, 2, 3]
    result_arr = add_one(arr)
    print *, 'add_one([1,2,3]) =', result_arr

    val = square(5)
    print *, 'square(5) =', val

    call compute_sum(3, 7, val)
    print *, 'compute_sum(3,7) =', val

    arr = [10, 20, 30]
    call double_value(arr)
    print *, 'double_value([10,20,30]) =', arr

    val = factorial(5)
    print *, 'factorial(5) =', val

    print *, 'All tests completed successfully'
end program test_pure_elemental
