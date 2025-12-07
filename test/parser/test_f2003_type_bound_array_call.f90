program test_f2003_type_bound_array_call
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_context_t, transform_with_context, &
        & INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lower_output
    type(transform_context_t) :: ctx
    logical :: all_passed

    all_passed = .true.

    call read_example('examples/f90/f2003_type_bound_array_call.f90', source_code)

    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = 'f2003_type_bound_array_call'

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform error: ' // trim(error_msg)
        error stop 1
    end if

    if (.not. allocated(output_code)) then
        write (error_unit, '(A)') 'FAIL: no output produced'
        error stop 1
    end if

    lower_output = to_lower(output_code)

    call assert_contains(lower_output, 'type :: shape_t', &
        & 'FAIL: type definition not preserved', all_passed)

    call assert_contains(lower_output, 'procedure :: compute', &
        & 'FAIL: type-bound procedure compute not preserved', all_passed)

    call assert_contains(lower_output, 'procedure :: display', &
        & 'FAIL: type-bound procedure display not preserved', all_passed)

    call assert_contains(lower_output, 'call shapes(i)%compute', &
        & 'FAIL: type-bound call with array index not preserved', all_passed)

    call assert_contains(lower_output, 'call shapes(i)%display', &
        & 'FAIL: second type-bound call with array index not preserved', all_passed)

    if (all_passed) then
        print *, 'PASS: F2003 type-bound array call constructs parsed correctly'
    else
        error stop 1
    end if

contains

    include '../common/cli_io_reader.inc'

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

    subroutine assert_contains(text, pattern, failure_message, passed)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: failure_message
        logical, intent(inout) :: passed

        if (index(text, pattern) == 0) then
            write (error_unit, '(A)') trim(failure_message)
            passed = .false.
        end if
    end subroutine assert_contains

end program test_f2003_type_bound_array_call
