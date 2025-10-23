program test_issue_1748_legacy_implied_do
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer :: arr(10), i
    integer :: squares(5)
    real(dp) :: evens(10)

    ! Test basic legacy implied-DO (issue 1748 reproducer)
    arr = (/ (i, i=1,10) /)
    if (arr(1) /= 1) then
        print *, "arr(1) should be 1, got", arr(1)
        stop 1
    end if
    if (arr(10) /= 10) then
        print *, "arr(10) should be 10, got", arr(10)
        stop 1
    end if

    ! Test with expression
    squares = (/ (i * i, i=1, 5) /)
    if (squares(1) /= 1) then
        print *, "squares(1) should be 1, got", squares(1)
        stop 1
    end if
    if (squares(5) /= 25) then
        print *, "squares(5) should be 25, got", squares(5)
        stop 1
    end if

    ! Test with step parameter
    evens = (/ (2.0d0 * i, i=1, 10) /)
    if (abs(evens(1) - 2.0d0) > 1.0d-10) then
        print *, "evens(1) should be 2.0, got", evens(1)
        stop 1
    end if
    if (abs(evens(10) - 20.0d0) > 1.0d-10) then
        print *, "evens(10) should be 20.0, got", evens(10)
        stop 1
    end if

    print *, "All tests passed!"
end program test_issue_1748_legacy_implied_do
