! Valid neighbor of enum_3: enumerators initialized with integer expressions,
! including a signed literal and a kind-suffixed literal.
program enum_3_valid
    implicit none

    enum, bind(c)
        enumerator :: red, black = 2
        enumerator :: blue = -3
        enumerator :: white = 7_4
    end enum

    print *, "ok"
end program enum_3_valid
