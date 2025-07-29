program test_implied_do_constructor
    implicit none
    integer :: i, j
    integer :: squares(5)
    integer :: evens(10)
    integer :: matrix(9)
    
    ! Test basic implied DO
    squares = [(i*i, i=1,5)]
    if (squares(1) /= 1) error stop "squares(1) should be 1"
    if (squares(2) /= 4) error stop "squares(2) should be 4"
    if (squares(3) /= 9) error stop "squares(3) should be 9"
    if (squares(4) /= 16) error stop "squares(4) should be 16"
    if (squares(5) /= 25) error stop "squares(5) should be 25"
    
    ! Test another implied DO
    evens = [(2*i, i=1,10)]
    if (evens(1) /= 2) error stop "evens(1) should be 2"
    if (evens(5) /= 10) error stop "evens(5) should be 10"
    if (evens(10) /= 20) error stop "evens(10) should be 20"
    
    ! Test nested implied DO
    matrix = [((i+j, i=1,3), j=1,3)]
    if (matrix(1) /= 2) error stop "matrix(1) should be 2"  ! i=1,j=1
    if (matrix(2) /= 3) error stop "matrix(2) should be 3"  ! i=2,j=1
    if (matrix(3) /= 4) error stop "matrix(3) should be 4"  ! i=3,j=1
    if (matrix(4) /= 3) error stop "matrix(4) should be 3"  ! i=1,j=2
    if (matrix(5) /= 4) error stop "matrix(5) should be 4"  ! i=2,j=2
    if (matrix(6) /= 5) error stop "matrix(6) should be 5"  ! i=3,j=2
    if (matrix(7) /= 4) error stop "matrix(7) should be 4"  ! i=1,j=3
    if (matrix(8) /= 5) error stop "matrix(8) should be 5"  ! i=2,j=3
    if (matrix(9) /= 6) error stop "matrix(9) should be 6"  ! i=3,j=3
    
    print *, "All tests passed!"
end program test_implied_do_constructor