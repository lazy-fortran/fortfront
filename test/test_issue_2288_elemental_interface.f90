program test_issue_2288_elemental_interface
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use transformation_api, only: transform_context_t, transform_with_context, &
        & INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx

    call read_example('examples/f90/issue_2288_elemental_interface.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2288_elemental_interface'

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

    if (index(output_code, 'elemental subroutine scale_value(') == 0) then
        write (error_unit, '(A)') &
            'FAIL: elemental subroutine declaration missing in output'
        error stop 1
    end if

    if (index(output_code, 'interface') == 0) then
        write (error_unit, '(A)') 'FAIL: interface block missing in output'
        error stop 1
    end if

    print *, 'PASS: Issue #2288 elemental interface parsed successfully'


contains


    include 'common/read_example.inc'
end program test_issue_2288_elemental_interface
