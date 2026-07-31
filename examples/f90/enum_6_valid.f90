! Valid neighbor of enum_6: the assignment lives in the executable part instead
! of inside the ENUM body.
program enum_6_valid
    implicit none
    integer :: i

    enum, bind(c)
        enumerator :: sun, mon = 2
        enumerator :: wed = 1
    end enum

    i = 2
    print *, i
end program enum_6_valid
