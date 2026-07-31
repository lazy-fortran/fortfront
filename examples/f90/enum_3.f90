! Invalid: an enumerator must be initialized with an integer expression
! (F2003 4.6). A real or character initializer is not allowed.
program enum_3
    implicit none

    enum, bind(c)
        enumerator :: red, black = 2.2
        enumerator :: blue = "x"
    end enum

end program enum_3
