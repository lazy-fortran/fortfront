program test_issue_2249_keyworded_write
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: has_fmt_write, has_format_write, has_positional_write

    call read_example('examples/f90/issue_2249_keyworded_write.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'issue_2249_keyworded_write'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') &
            'FAIL: transform_with_context returned error: ' // trim(error_msg)
        error stop 1
    end if

    has_fmt_write = index(output_code, 'write(*, ''(I5)'') x') > 0
    has_format_write = index(output_code, 'write(*, ''(I5)'') y') > 0
    has_positional_write = index(output_code, 'write(*, ''(I5)'') z') > 0

    if (.not. has_fmt_write) then
        write (error_unit, '(A)') 'FAIL: fmt= WRITE statement missing in output'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_format_write) then
        write (error_unit, '(A)') &
            'FAIL: format= WRITE statement missing in output'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. has_positional_write) then
        write (error_unit, '(A)') 'FAIL: positional WRITE missing in output'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: Issue #2249 keyworded WRITE statements preserved'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2249_keyworded_write
