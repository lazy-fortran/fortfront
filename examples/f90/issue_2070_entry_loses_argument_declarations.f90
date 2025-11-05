program test_entry_statement
    implicit none
    integer :: result

    result = func1(5)
    print *, "func1(5) =", result

    result = func2(3, 4)
    print *, "func2(3,4) =", result
end program test_entry_statement

function func1(x) result(res)
    integer, intent(in) :: x
    integer :: res
    res = x * 2
    return
    entry func2(a, b) result(res)
    integer, intent(in) :: a, b
    res = a + b
end function func1
