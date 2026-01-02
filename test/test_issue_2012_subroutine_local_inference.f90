program test_issue_2012_subroutine_local_inference
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    call test_subroutine_local_variable()
    print *, 'PASS: Issue 2012 subroutine local variable inference'

contains

    include 'common/read_example.inc'


    subroutine test_subroutine_local_variable()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        call read_example('examples/lf/issue_2012_subroutine_local_not_inferred.lf', &
                          source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: Transformation failed: ' // &
                trim(error_msg)
            error stop 1
        end if

        if (index(output, 'integer :: temp') == 0) then
            write (error_unit, '(A)') "FAIL: Local variable 'temp' not declared"
            call write_output_snippet(output)
            error stop 1
        end if

        if (index(output, 'subroutine swap') == 0) then
            write (error_unit, '(A)') 'FAIL: Subroutine not found in output'
            error stop 1
        end if

        if (index(output, 'implicit none') == 0) then
            write (error_unit, '(A)') 'FAIL: implicit none not found'
            error stop 1
        end if
    end subroutine test_subroutine_local_variable

    subroutine write_output_snippet(output)
        character(len=*), intent(in) :: output
        integer :: n

        n = min(len(output), 4000)
        write (error_unit, '(A)') '---- output snippet begin ----'
        if (n > 0) then
            write (error_unit, '(A)') output(1:n)
        end if
        if (len(output) > n) then
            write (error_unit, '(A,I0,A)') '---- truncated; total chars=', &
                len(output), ' ----'
        end if
        write (error_unit, '(A)') '---- output snippet end ----'
    end subroutine write_output_snippet

end program test_issue_2012_subroutine_local_inference
