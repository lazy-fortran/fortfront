! Valid neighbor of enum_7: two ENUM definitions in sequence rather than one
! nested inside the other.
program enum_7_valid
    implicit none

    enum, bind(c)
        enumerator :: sun, mon = 2
        enumerator :: wed = 1
    end enum

    enum, bind(c)
        enumerator :: apple, mango
    end enum

    print *, "ok"
end program enum_7_valid
