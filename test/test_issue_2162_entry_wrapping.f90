program test_issue_2162_entry_wrapping
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    call verify_entry_not_internal()
    print *, '[PASS] entry procedures remain external'

contains

    include 'common/read_example.inc'


    subroutine verify_entry_not_internal()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg

        call read_example('examples/f90/issue_2162_entry_wrapping.f90', &
                          input_code)
        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') 'FAIL: transformation failed -> ' // &
                    trim(error_msg)
                error stop 1
            end if
        end if

        if (.not. allocated(output_code)) then
            write (error_unit, '(A)') 'FAIL: transformation produced no output'
            error stop 1
        end if

        if (index(output_code, 'program main') > 0) then
            write (error_unit, '(A)') &
                'FAIL: entry procedure wrapped in internal program'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'entry triple_value') == 0) then
            write (error_unit, '(A)') 'FAIL: entry statement missing'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if

        if (index(output_code, 'function scale_value') == 0) then
            write (error_unit, '(A)') 'FAIL: function definition missing'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if
    end subroutine verify_entry_not_internal

end program test_issue_2162_entry_wrapping

