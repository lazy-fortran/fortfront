program test_combined_features
    implicit none

    print *, 'Testing combined features'
    call test_function()
    call test_subroutine()

contains

    subroutine test_function()
        integer :: result
        result = factorial(5)
        print *, 'Factorial(5) =', result
    end subroutine test_function

    recursive integer function factorial(n) result(res)
        integer, intent(in) :: n
        if (n <= 1) then
            res = 1
        else
            res = n * factorial(n - 1)
        end if
    end function factorial

    subroutine test_subroutine()
        integer, dimension(5) :: arr
        arr = [1, 2, 3, 4, 5]
        print *, 'Sum:', sum(arr)
    end subroutine test_subroutine

end program test_combined_features
