program elemental_pointer_1
    ! INVALID: F2008 12.8.1. The result of an ELEMENTAL function shall be
    ! scalar and shall not have the POINTER attribute.
    !
    ! The gfortran fixture spells POINTER as a separate attribute statement
    ! ("POINTER :: LL"). FortFront's parser currently drops standalone
    ! attribute statements, so the constraint is exercised here through the
    ! type-declaration spelling of the same attribute.
    implicit none

contains

    elemental function ll(i)
        integer, intent(in) :: i
        integer, pointer :: ll
    end function ll

end program elemental_pointer_1
