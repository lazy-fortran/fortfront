program elemental_args_check_2_valid
    ! VALID neighbour of elemental_args_check_2.f90. The dummy procedure moves
    ! to a plain PURE subroutine, and the ELEMENTAL subroutine keeps only
    ! scalar data dummies.
    implicit none

contains

    pure subroutine s1(i, f)
        integer, intent(in) :: i
        interface
            pure integer function f(j)
                integer, intent(in) :: j
            end function f
        end interface
    end subroutine s1

    pure elemental subroutine s2(i, j)
        integer, intent(in) :: i
        integer, intent(out) :: j
        j = i + 1
    end subroutine s2

end program elemental_args_check_2_valid
