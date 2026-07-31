! Invalid: an ENUM definition may not be nested inside another ENUM body
! (F2003 R460 admits only enumerator-def-stmt between ENUM and END ENUM).
program enum_7
    implicit none

    enum, bind(c)
        enumerator :: sun, mon = 2
        enum, bind(c)
            enumerator :: apple, mango
        end enum
        enumerator :: wed = 1
    end enum

end program enum_7
