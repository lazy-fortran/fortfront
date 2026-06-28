program test_issue_2278_return_identifier
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx

    call read_example('examples/f90/issue_2278_return_identifier.f90', &
        source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = "issue_2278_return"

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') &
            'FAIL: transform_with_context returned error: ' // trim(error_msg)
        error stop 1
    end if

    if (index(output_code, 'integer :: return') == 0) then
        write (error_unit, '(A)') &
            'FAIL: integer declaration for return missing in output'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (index(output_code, 'return = 42') == 0) then
        write (error_unit, '(A)') 'FAIL: assignment "return = 42" missing'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (index(output_code, 'real :: return') > 0) then
        write (error_unit, '(A)') 'FAIL: unexpected type change to REAL'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: Issue #2278 - return identifier preserved'


contains


    include 'common/read_example.inc'
end program test_issue_2278_return_identifier
