program test_issue_2280_statement_function
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_context_t, transform_with_context
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx

    call read_example('examples/f90/issue_2280_statement_function.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2280_statement_function'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context error: ' // &
            trim(error_msg)
        error stop 1
    end if

    if (output_code /= source_code) then
        write (error_unit, '(A)') 'FAIL: statement function did not round-trip'
        error stop 1
    end if

    print *, 'PASS: statement function round-trips without bogus rewrite'

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

end program test_issue_2280_statement_function
