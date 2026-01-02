program test_multiline_statement_if
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: context

    call read_example('examples/f90/if_multiline_guard.f90', input_code)

    context%input_mode = INPUT_MODE_STANDARD
    context%has_filename = .true.
    context%source_name = 'test_multiline_statement_if'

    call transform_with_context(input_code, output_code, error_msg, context)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation reported an error'
        write (error_unit, '(A)') trim(error_msg)
        error stop 1
    end if

    if (index(output_code, 'if_multiline_guard') == 0) then
        write (error_unit, '(A)') 'FAIL: output missing program name'
        error stop 1
    end if

    print *, 'PASS: multiline statement-if preserves syntax'


contains


    include '../common/read_example.inc'
end program test_multiline_statement_if
