program elemental_result_1
    ! INVALID: F2008 12.8.1. The result of an ELEMENTAL function shall be
    ! scalar; an array result is not allowed.
    implicit none

contains

    elemental function ll(i)
        integer, intent(in) :: i
        integer :: ll(2)
    end function ll

end program elemental_result_1
