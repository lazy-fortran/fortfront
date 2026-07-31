program elemental_args_check_2
    ! INVALID: F2003 C1277. A dummy argument of an ELEMENTAL procedure shall
    ! be a data object, so a dummy procedure is not allowed.
    implicit none

contains

    pure elemental subroutine s1(i, f)
        integer, intent(in) :: i
        interface
            pure integer function f(j)
                integer, intent(in) :: j
            end function f
        end interface
    end subroutine s1

end program elemental_args_check_2
