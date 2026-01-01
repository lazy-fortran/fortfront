program test_issue_2694_strict_zero_sized_array_literals
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_context_t, &
                                  transform_with_context, &
                                  INPUT_MODE_LAZY, &
                                  OPERATING_MODE_INFER, &
                                  OPERATING_MODE_STRICT
    implicit none

    call check_example( &
        'examples/lf/issue_2694_strict_zero_sized_array_constructor.lf')
    call check_example( &
        'examples/lf/issue_2694_strict_zero_iter_implied_do.lf')

    write (error_unit, '(a)') &
        'PASS: strict mode treats known empty array constructors as size-known'

contains

    include '../../common/cli_io_reader.inc'

    subroutine check_example(path)
        character(len=*), intent(in) :: path

        character(len=:), allocatable :: error_msg
        character(len=:), allocatable :: output
        character(len=:), allocatable :: source
        type(transform_context_t) :: ctx

        call read_example(path, source)

        ctx%input_mode = INPUT_MODE_LAZY
        ctx%has_filename = .false.

        ctx%operating_mode = OPERATING_MODE_INFER
        call transform_with_context(source, output, error_msg, ctx)
        call assert_no_error(error_msg)

        ctx%operating_mode = OPERATING_MODE_STRICT
        call transform_with_context(source, output, error_msg, ctx)
        call assert_contains(error_msg, 'forbids automatic allocatable array '// &
                             'reallocation')
    end subroutine check_example

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(a)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

    subroutine assert_no_error(msg)
        character(len=:), allocatable, intent(in) :: msg

        if (.not. allocated(msg)) return
        if (len_trim(msg) == 0) return
        write (error_unit, '(a)') 'FAIL: unexpected error message:'
        write (error_unit, '(a)') trim(msg)
        error stop 1
    end subroutine assert_no_error

    subroutine assert_contains(msg, needle)
        character(len=:), allocatable, intent(in) :: msg
        character(len=*), intent(in) :: needle

        if (.not. allocated(msg)) then
            write (error_unit, '(a)') 'FAIL: expected error message'
            error stop 1
        end if
        if (index(msg, needle) == 0) then
            write (error_unit, '(a)') 'FAIL: expected message to contain: ' // &
                trim(needle)
            write (error_unit, '(a)') 'Got:'
            write (error_unit, '(a)') trim(msg)
            error stop 1
        end if
    end subroutine assert_contains

end program test_issue_2694_strict_zero_sized_array_literals
