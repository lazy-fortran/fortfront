! Valid neighbor of enum_8: enumerator values that fit the C int kind, taken at
! both ends of the range.
program enum_8_valid
    implicit none

    enum, bind(c)
        enumerator :: pp, qq = 2147483646, rr
    end enum

    enum, bind(c)
        enumerator :: p, q = -2147483647, r
    end enum

    print *, "ok"
end program enum_8_valid
