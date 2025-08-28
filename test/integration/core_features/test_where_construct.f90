program test_where_construct
    implicit none
    real :: a(5) = [1.0, -2.0, 3.0, -4.0, 5.0]
    real :: b(5)
    real :: c(5)
    integer :: i
    
    ! Test basic WHERE construct
    where (a > 0.0)
        b = sqrt(a)
    elsewhere
        b = 0.0
    end where
    
    ! Check results
    if (abs(b(1) - 1.0) > 1e-6) then
        print *, "WHERE test failed: b(1) should be 1.0"
        stop 1
    end if
    if (abs(b(2) - 0.0) > 1e-6) then
        print *, "WHERE test failed: b(2) should be 0.0"
        stop 1
    end if
    if (abs(b(3) - sqrt(3.0)) > 1e-6) then
        print *, "WHERE test failed: b(3) should be sqrt(3.0)"
        stop 1
    end if
    if (abs(b(4) - 0.0) > 1e-6) then
        print *, "WHERE test failed: b(4) should be 0.0"
        stop 1
    end if
    if (abs(b(5) - sqrt(5.0)) > 1e-6) then
        print *, "WHERE test failed: b(5) should be sqrt(5.0)"
        stop 1
    end if
    
    ! Test single-line WHERE
    where (a > 0.0) c = 1.0/a
    where (a <= 0.0) c = -1.0
    
    ! Check results
    if (abs(c(1) - 1.0) > 1e-6) then
        print *, "Single-line WHERE test failed: c(1) should be 1.0"
        stop 1
    end if
    if (abs(c(2) - (-1.0)) > 1e-6) then
        print *, "Single-line WHERE test failed: c(2) should be -1.0"
        stop 1
    end if
    if (abs(c(3) - 1.0/3.0) > 1e-6) then
        print *, "Single-line WHERE test failed: c(3) should be 1/3"
        stop 1
    end if
    if (abs(c(4) - (-1.0)) > 1e-6) then
        print *, "Single-line WHERE test failed: c(4) should be -1.0"
        stop 1
    end if
    if (abs(c(5) - 0.2) > 1e-6) then
        print *, "Single-line WHERE test failed: c(5) should be 0.2"
        stop 1
    end if
    
    print *, "All tests passed!"
end program test_where_construct