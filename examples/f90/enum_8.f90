! Invalid: an enumerator value must fit the kind of the enumeration, which for
! a bind(c) enumeration is the C int kind (F2003 4.6).
program enum_8
    implicit none

    enum, bind(c)
        enumerator :: pp, qq = 4294967295, rr
    end enum

    enum, bind(c)
        enumerator :: p, q = 4294967299_8, r
    end enum

end program enum_8
