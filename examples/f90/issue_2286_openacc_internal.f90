program issue_2286_openacc_internal
    implicit none
    call run_parallel_loop()
contains
    subroutine run_parallel_loop()
        integer :: i
        !$acc parallel loop
        do i = 1, 3
            call emit_value(i)
        end do
        !$acc end parallel loop
    end subroutine run_parallel_loop

    subroutine emit_value(value)
        use, intrinsic :: iso_fortran_env, only: output_unit
        implicit none
        integer, intent(in) :: value
        write (output_unit, '(I0)') value
    end subroutine emit_value
end program issue_2286_openacc_internal
