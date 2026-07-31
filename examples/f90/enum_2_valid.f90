! Valid neighbor of enum_2: the ENUM body holds only ENUMERATOR statements and
! the initialized enumerator uses the required "::" separator.
program enum_2_valid
    implicit none
    integer :: x

    enum, bind(c)
        enumerator :: red, black
        enumerator green, yellow
        enumerator :: blue = 1
    end enum

    x = 0
    print *, x
end program enum_2_valid
