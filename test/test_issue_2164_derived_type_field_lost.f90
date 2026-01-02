program test_issue_2164_derived_type_field_lost
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use string_utils_mod, only: to_lower
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: found_data_field, found_inner_field, found_correct_assignment

    ! Read the example file
    call read_example('examples/f90/issue_playtest5_derived_type_field_lost.f90', source_code)

    ! Transform using standard Fortran mode (round-trip)
    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = "issue_2164_test"

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transform_with_context returned error: ' // trim(error_msg)
        error stop 1
    end if

    ! Check that output contains both fields in the derived type definition
    found_inner_field = .false.
    found_data_field = .false.
    found_correct_assignment = .false.

    ! Look for "type(inner_type) :: inner" in the type definition
    if (index(output_code, 'type(inner_type) :: inner') > 0 .or. &
        index(output_code, 'type(inner_type):: inner') > 0 .or. &
        index(output_code, 'type (inner_type) :: inner') > 0) then
        found_inner_field = .true.
    end if

    ! Look for "real :: data" in the type definition
    if (index(output_code, 'real :: data') > 0 .or. &
        index(output_code, 'real:: data') > 0 .or. &
        index(output_code, 'real ::data') > 0 .or. &
        index(output_code, 'real::data') > 0) then
        found_data_field = .true.
    end if

    ! Look for correct assignment "obj%data = 3.14" (not "obj = 3.14")
    if (index(output_code, 'obj%data') > 0) then
        found_correct_assignment = .true.
    end if

    ! Report results
    if (.not. found_inner_field) then
        write (error_unit, '(A)') 'FAIL: inner field not found in output'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. found_data_field) then
        write (error_unit, '(A)') 'FAIL: data field not found in output (BUG #2164)'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. found_correct_assignment) then
        write (error_unit, '(A)') 'FAIL: correct assignment obj%data not found (BUG #2164)'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: Issue #2164 test passed'


contains


    include 'common/read_example.inc'
end program test_issue_2164_derived_type_field_lost
