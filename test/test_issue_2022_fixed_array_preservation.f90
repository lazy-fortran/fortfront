program test_issue_2022_fixed_array_preservation
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_with_context, transform_context_t
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use string_utils_mod, only: to_lower
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    character(:), allocatable :: lowered_output
    type(transform_context_t) :: context

    print *, '=== Issue #2022: fixed-size array preservation ==='

    call read_example('examples/f90/issue_2022_fixed_array_to_unallocated.f90', &
                      input_code)

    context%input_mode = INPUT_MODE_STANDARD
    context%has_filename = .true.
    context%source_name = "test_issue_2022_input"

    call transform_with_context(input_code, output_code, error_msg, context)

    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: transformation reported an error'
        print *, trim(error_msg)
        error stop 1
    end if

    lowered_output = to_lower(output_code)

    ! Verify fixed-size array is preserved
    if (index(lowered_output, 'integer :: arr(5)') == 0) then
        print *, 'FAIL: fixed-size array arr(5) not preserved'
        print *, 'Output:' // new_line('A') // trim(output_code)
        error stop 1
    end if

    ! Verify it's NOT converted to allocatable
    if (index(lowered_output, 'allocatable :: arr') > 0 .or. &
        index(lowered_output, 'arr(:)') > 0) then
        print *, 'FAIL: array incorrectly converted to allocatable'
        print *, 'Output:' // new_line('A') // trim(output_code)
        error stop 1
    end if

    print *, 'PASS: fixed-size array preserved correctly'

contains

    include 'common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            print *, 'FAIL: failed to read ', trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_2022_fixed_array_preservation
