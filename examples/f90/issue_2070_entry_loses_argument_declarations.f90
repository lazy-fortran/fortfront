! Test case for issue #2567: ENTRY statement loses argument declarations
! This test verifies that procedures with ENTRY statements are NOT
! incorrectly placed inside a contains block (which is illegal).
! The function func1 has an ENTRY point func2 with different arguments.
! Both entry points share the same result variable but take different args.

program test_entry_statement
    implicit none
    integer :: result_val
    integer, external :: func1, func2

    result_val = func1(5)
    print *, "func1(5) =", result_val

    result_val = func2(3, 4)
    print *, "func2(3,4) =", result_val
end program test_entry_statement

function func1(x) result(res)
    integer, intent(in) :: x
    integer :: res
    integer, intent(in) :: a, b
    res = x * 2
    return
    entry func2(a, b) result(res)
    res = a + b
end function func1
