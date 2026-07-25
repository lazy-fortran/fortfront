program elemental_pointer_1_valid
    ! VALID neighbour of elemental_pointer_1.f90. The ELEMENTAL function has a
    ! scalar non-pointer result, and the POINTER result appears only on an
    ! ordinary function.
    implicit none

contains

    elemental function ll(i)
        integer, intent(in) :: i
        integer :: ll
        ll = i + 1
    end function ll

    function mm(i) result(res)
        integer, intent(in) :: i
        integer, pointer :: res
        allocate (res)
        res = i
    end function mm

end program elemental_pointer_1_valid
