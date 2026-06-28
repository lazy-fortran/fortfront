program issue_2285_openmp_directives
    implicit none
    integer :: i
    !$omp parallel do
    do i = 1, 2
        call emit_value(i)
    end do
    !$omp end parallel do
contains
    subroutine emit_value(value)
        use, intrinsic :: iso_fortran_env, only: output_unit
        implicit none
        integer, intent(in) :: value
        write (output_unit, '(I0)') value
    end subroutine emit_value
end program issue_2285_openmp_directives
