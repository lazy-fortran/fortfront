program test_issue_2289_impure_interface
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
        & iostat_end, iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code, output_code, error_msg
    type(transform_context_t) :: ctx
    logical :: has_impure_declaration

    call read_example('examples/f90/issue_2289_impure_interface.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2289_impure_interface'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    has_impure_declaration = index(output_code, 'impure subroutine log_state') > 0

    if (.not. has_impure_declaration) then
        write (error_unit, '(A)') 'FAIL: impure interface declaration missing'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: impure interface attributes survive round-trip'


contains


    include 'common/read_example.inc'
end program test_issue_2289_impure_interface
