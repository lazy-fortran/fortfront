program test_issue_2022_fixed_array_preservation
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_with_context, transform_context_t
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use string_utils_mod, only: to_lower
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lowered_output
    type(transform_context_t) :: context

    call read_example('examples/f90/issue_2022_fixed_array_to_unallocated.f90', &
                      input_code)

    context%input_mode = INPUT_MODE_STANDARD
    context%has_filename = .true.
    context%source_name = 'test_issue_2022_input'

    call transform_with_context(input_code, output_code, error_msg, context)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation reported an error'
        write (error_unit, '(A)') trim(error_msg)
        error stop 1
    end if

    lowered_output = to_lower(output_code)

    if (index(lowered_output, 'integer :: arr(5)') == 0) then
        write (error_unit, '(A)') 'FAIL: fixed-size array arr(5) not preserved'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(lowered_output, 'allocatable :: arr') > 0 .or. &
        index(lowered_output, 'arr(:)') > 0) then
        write (error_unit, '(A)') 'FAIL: array incorrectly converted to allocatable'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    print *, 'PASS: fixed-size array preserved correctly'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2022_fixed_array_preservation
