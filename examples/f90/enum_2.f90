! Invalid: an ENUM body may contain only ENUMERATOR statements (F2003 R460),
! and an enumerator initializer requires the "::" separator.
program enum_2
    implicit none

    enum, bind(c)
        enumerator :: red, black
        integer :: x
        enumerator blue = 1
    end enum

end program enum_2
