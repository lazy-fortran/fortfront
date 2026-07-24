program compiler_resolved_expression_kinds
    implicit none

    integer, parameter :: dp = 8
    integer, parameter :: wp = 16
    integer(1) :: i1
    integer(2) :: i2
    integer(4) :: i4
    integer(8) :: i8
    real(4) :: r4
    real(dp) :: r8
    real(wp) :: r16
    real(8) :: r8_explicit
    real(16) :: r16_explicit
    real(wp) :: mixed
    complex(8) :: z8
    real(8) :: real_z8
    real(4) :: real_z4
    real(8) :: imag_z8
    real(8) :: abs_z8

    i1 = 1_1
    i2 = 2_2
    i4 = 4_4
    i8 = 8_8
    r4 = 1.0_4
    r8 = 1.0_dp
    r16 = -1.0_wp
    r8_explicit = 2.0_8
    r16_explicit = 2.0_16
    mixed = r4 + r16
    z8 = cmplx(1.0_8, 2.0_8, kind=8)
    real_z8 = real(z8)
    real_z4 = real(z8, kind=4)
    imag_z8 = aimag(z8)
    abs_z8 = abs(z8)
    print *, f16(r4)

contains

    real(wp) function f16(x)
        real(4), intent(in) :: x

        f16 = x + 0.0_wp
    end function f16
end program compiler_resolved_expression_kinds
