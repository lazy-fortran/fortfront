program test_issue_2283_nested_modules
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
        & iostat_end, iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code, output_code, error_msg
    type(transform_context_t) :: ctx
    logical :: has_module_value
    logical :: has_set_subroutine
    logical :: has_touch_subroutine

    ! Note: The original issue #2283 reported "nested modules" as valid Fortran,
    ! but nested module syntax is NOT valid in any Fortran standard.
    ! This test now validates that a valid module with contained procedures
    ! survives the round-trip correctly.

    call read_example('examples/f90/issue_2283_nested_modules.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2283_nested_modules'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    ! Verify the module structure is preserved
    has_module_value = index(output_code, 'module_value') > 0
    has_set_subroutine = index(output_code, 'set_module_value') > 0
    has_touch_subroutine = index(output_code, 'touch_value') > 0

    if (.not. has_module_value) then
        write (error_unit, '(A)') 'FAIL: module_value lost during round-trip'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_set_subroutine) then
        write (error_unit, '(A)') 'FAIL: set_module_value subroutine lost'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_touch_subroutine) then
        write (error_unit, '(A)') 'FAIL: touch_value subroutine lost'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: module with contained procedures survives round-trip'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2283_nested_modules
