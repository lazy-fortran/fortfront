! Test case for issue #2457: Logical operator type mismatch
! This demonstrates VALID Fortran that should roundtrip correctly
program test_logical_operators
    implicit none
    integer :: x, y
    character(len=10) :: str
    logical :: result1, result2, result3

    x = 5
    y = 10
    str = "test"

    ! VALID: Logical operators on LOGICAL expressions
    result1 = (x > 0) .and. (y > 0)
    result2 = (len_trim(str) > 0) .and. (str == "test")
    result3 = .not. (x == y)

    print *, "Result 1:", result1
    print *, "Result 2:", result2
    print *, "Result 3:", result3

    ! VALID: Using bitwise intrinsics for integer operations
    if (iand(x, y) /= 0) then
        print *, "Bitwise AND is non-zero"
    end if
end program test_logical_operators
