program test_issue_2291_operator_continuation
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_context_t, transform_with_context
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    integer :: assign_pos

    call read_example('examples/f90/issue_2291_operator_continuation.f90', &
        & source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2291_operator_continuation'

    call transform_with_context(source_code, output_code, error_msg, ctx)
    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    assign_pos = index(output_code, 'total = 40 + 2')
    if (assign_pos == 0) then
        write (error_unit, '(A)') 'FAIL: missing collapsed assignment in output'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: Issue #2291 operator continuation preserved'


contains


    include 'common/read_example.inc'
end program test_issue_2291_operator_continuation
