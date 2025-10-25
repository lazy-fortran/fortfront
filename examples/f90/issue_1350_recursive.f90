! Minimal reproducer for issue #1350: RECURSIVE keyword dropped, ELSE branch body missing
recursive function factorial(n) result(f)
    integer, intent(in) :: n
    integer :: f

    if (n <= 1) then
        f = 1
    else
        f = n * factorial(n - 1)
    end if
end function factorial

program test_recursive
    implicit none
    integer, external :: factorial
    print *, factorial(5)
end program test_recursive
