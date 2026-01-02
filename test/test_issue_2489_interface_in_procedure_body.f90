program test_issue_2489_interface_in_procedure_body
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_context_t, transform_with_context, &
        & INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lower_output
    type(transform_context_t) :: ctx

    call read_example('examples/f90/issue_2489_interface_in_procedure.f90', &
        & source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2489_interface_in_procedure'

    call transform_with_context(source_code, output_code, error_msg, ctx)
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            & trim(error_msg)
        error stop 1
    end if

    if (.not. allocated(output_code)) then
        write (error_unit, '(A)') 'FAIL: transform_with_context produced no output'
        error stop 1
    end if

    lower_output = to_lower(output_code)

    call assert_contains(lower_output, 'interface', &
        & 'FAIL: interface block not preserved in output')
    call assert_contains(lower_output, 'end interface', &
        & 'FAIL: end interface not preserved in output')
    call assert_contains(lower_output, 'subroutine ext_sub', &
        & 'FAIL: interface procedure ext_sub not preserved in output')
    call assert_contains(lower_output, 'function ext_func', &
        & 'FAIL: interface procedure ext_func not preserved in output')

    print *, 'PASS: Issue #2489 interface blocks in procedure bodies parsed'

contains

    include 'common/read_example.inc'


    subroutine assert_contains(text, pattern, failure_message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: failure_message

        if (index(text, pattern) == 0) then
            write (error_unit, '(A)') trim(failure_message)
            write (error_unit, '(A)') 'Output was:'
            write (error_unit, '(A)') trim(text)
            error stop 1
        end if
    end subroutine assert_contains

end program test_issue_2489_interface_in_procedure_body
