! Invalid: an assignment statement is not an ENUM definition statement, so it
! may not appear inside an ENUM body (F2003 R460).
program enum_6
    implicit none
    integer :: i = 1

    enum, bind(c)
        enumerator :: sun, mon = 2
        i = 2
        enumerator :: wed = 1
    end enum

end program enum_6
