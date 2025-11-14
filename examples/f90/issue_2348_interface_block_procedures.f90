program issue_2348_interface_block_procedures
    use, intrinsic :: iso_fortran_env, only: dp => real64, sp => real32
    implicit none

    interface check_real
        procedure :: check_r4
        procedure check_r8
    end interface check_real

    interface math_helpers
        integer function integer_abs_value(x)
            integer, intent(in) :: x
        end function integer_abs_value

        double precision function double_abs_value(x)
            double precision, intent(in) :: x
        end function double_abs_value

        logical function is_positive(x)
            real, intent(in) :: x
        end function is_positive

        character(len=1) function status_flag(flag)
            logical, intent(in) :: flag
        end function status_flag
    end interface math_helpers

contains

    real(sp) function check_r4(value)
        real(sp), intent(in) :: value
        check_r4 = value
    end function check_r4

    real(dp) function check_r8(value)
        real(dp), intent(in) :: value
        check_r8 = value
    end function check_r8

end program issue_2348_interface_block_procedures
