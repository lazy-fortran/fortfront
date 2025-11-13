program test_issue_2283_nested_modules
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
        & iostat_end, iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code, output_code, error_msg
    type(transform_context_t) :: ctx
    logical :: has_inner_module

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

    has_inner_module = index(output_code, 'module issue_2283_inner') > 0 .and. &
        & index(output_code, 'end module issue_2283_inner') > 0

    if (.not. has_inner_module) then
        write (error_unit, '(A)') 'FAIL: nested module lost during round-trip'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: nested modules survive round-trip'

contains

    include 'common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_2283_nested_modules
