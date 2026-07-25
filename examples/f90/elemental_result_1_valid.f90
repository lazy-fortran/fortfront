program elemental_result_1_valid
    ! VALID neighbour of elemental_result_1.f90. The ELEMENTAL function keeps a
    ! scalar result; the array-valued form is written as an ordinary function.
    implicit none

contains

    elemental function ll(i)
        integer, intent(in) :: i
        integer :: ll
        ll = i*2
    end function ll

    function mm(i)
        integer, intent(in) :: i
        integer :: mm(2)
        mm(1) = i
        mm(2) = i + 1
    end function mm

end program elemental_result_1_valid
