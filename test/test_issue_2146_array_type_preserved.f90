program test_issue_2146_array_type_preserved
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lowered_output
    type(transform_context_t) :: context

    call read_example('examples/f90/issue_playtest5_array_type_changed_real_to_int.f90', &
                      input_code)

    context%input_mode = INPUT_MODE_STANDARD
    context%has_filename = .true.
    context%source_name = 'test_issue_2146_input'

    call transform_with_context(input_code, output_code, error_msg, context)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation reported an error'
        write (error_unit, '(A)') trim(error_msg)
        error stop 1
    end if

    lowered_output = to_lower(output_code)

    if (index(lowered_output, 'real(dp), allocatable :: arr(:)') == 0 .and. &
        index(lowered_output, 'real, allocatable :: arr(:)') == 0) then
        write (error_unit, '(A)') 'FAIL: arr lost real allocatable declaration'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(lowered_output, 'integer, allocatable :: arr(:)') > 0) then
        write (error_unit, '(A)') 'FAIL: arr incorrectly declared integer'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    print *, 'PASS: Arr stays real allocatable in round-trip'


contains


    include 'common/read_example.inc'
end program test_issue_2146_array_type_preserved
