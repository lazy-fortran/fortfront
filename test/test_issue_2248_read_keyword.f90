program test_issue_2248_read_keyword
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: has_fmt_read, has_format_read, has_positional_read

    call read_example('examples/f90/issue_2248_read_keyword.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2248_read_keyword'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') &
            'FAIL: transform_with_context returned error: ' // trim(error_msg)
        error stop 1
    end if

    has_fmt_read = index(output_code, 'read(*, ''(I5)'') x') > 0
    has_format_read = index(output_code, 'read(*, ''(I5)'') y') > 0
    has_positional_read = index(output_code, 'read(*, ''(I5)'') z') > 0

    if (.not. has_fmt_read) then
        write (error_unit, '(A)') 'FAIL: fmt= READ statement missing in output'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_format_read) then
        write (error_unit, '(A)') 'FAIL: format= READ statement missing in output'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_positional_read) then
        write (error_unit, '(A)') 'FAIL: positional READ statement missing in output'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: Issue #2248 keyworded READ statements preserved'

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

end program test_issue_2248_read_keyword
