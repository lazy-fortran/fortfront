program test_issue_2348_interface_block_procedures
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

    call read_example('examples/f90/issue_2348_interface_block_procedures.f90', &
        & source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2348_interface_block_procedures'

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

    call assert_contains(lower_output, 'procedure :: check_r4', &
        & 'FAIL: procedure check_r4 not preserved in output')
    call assert_contains(lower_output, 'procedure check_r8', &
        & 'FAIL: procedure check_r8 not preserved in output')
    call assert_contains(lower_output, 'integer function integer_abs_value', &
        & 'FAIL: integer return type missing for integer_abs_value')
    call assert_contains(lower_output, 'double precision function double_abs_value', &
        & 'FAIL: double precision return type missing for double_abs_value')
    call assert_contains(lower_output, 'logical function is_positive', &
        & 'FAIL: logical return type missing for is_positive')
    call assert_contains(lower_output, 'character(len=1) function status_flag', &
        & 'FAIL: character return type missing for status_flag')

    print *, 'PASS: Issue #2348 interface block procedures parsed successfully'

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'


    subroutine assert_contains(text, pattern, failure_message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: failure_message

        if (index(text, pattern) == 0) then
            write (error_unit, '(A)') trim(failure_message)
            error stop 1
        end if
    end subroutine assert_contains

end program test_issue_2348_interface_block_procedures
