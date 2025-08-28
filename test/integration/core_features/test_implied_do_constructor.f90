program test_implied_do_constructor
    implicit none
    integer :: i, j
    integer :: squares(5)
    integer :: evens(10)
    integer :: matrix(9)
    
    ! Test basic implied DO
    squares = [(i*i, i=1,5)]
    if (squares(1) /= 1) then
        print *, "squares(1) should be 1"
        stop 1
    end if
    if (squares(2) /= 4) then
        print *, "squares(2) should be 4"
        stop 1
    end if
    if (squares(3) /= 9) then
        print *, "squares(3) should be 9"
        stop 1
    end if
    if (squares(4) /= 16) then
        print *, "squares(4) should be 16"
        stop 1
    end if
    if (squares(5) /= 25) then
        print *, "squares(5) should be 25"
        stop 1
    end if
    
    ! Test another implied DO
    evens = [(2*i, i=1,10)]
    if (evens(1) /= 2) then
        print *, "evens(1) should be 2"
        stop 1
    end if
    if (evens(5) /= 10) then
        print *, "evens(5) should be 10"
        stop 1
    end if
    if (evens(10) /= 20) then
        print *, "evens(10) should be 20"
        stop 1
    end if
    
    ! Test nested implied DO
    matrix = [((i+j, i=1,3), j=1,3)]
    if (matrix(1) /= 2) then
        print *, "matrix(1) should be 2"
        stop 1
    end if
    if (matrix(2) /= 3) then
        print *, "matrix(2) should be 3"
        stop 1
    end if
    if (matrix(3) /= 4) then
        print *, "matrix(3) should be 4"
        stop 1
    end if
    if (matrix(4) /= 3) then
        print *, "matrix(4) should be 3"
        stop 1
    end if
    if (matrix(5) /= 4) then
        print *, "matrix(5) should be 4"
        stop 1
    end if
    if (matrix(6) /= 5) then
        print *, "matrix(6) should be 5"
        stop 1
    end if
    if (matrix(7) /= 4) then
        print *, "matrix(7) should be 4"
        stop 1
    end if
    if (matrix(8) /= 5) then
        print *, "matrix(8) should be 5"
        stop 1
    end if
    if (matrix(9) /= 6) then
        print *, "matrix(9) should be 6"
        stop 1
    end if
    
    print *, "All tests passed!"
end program test_implied_do_constructor