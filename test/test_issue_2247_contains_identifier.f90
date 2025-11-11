program test_issue_2247_contains_identifier
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: ctx
    logical :: found_assignment, found_print

    ! Read the example file
    call read_example('examples/f90/issue_2247_contains_identifier.f90', &
                      source_code)

    ! Transform using standard Fortran mode (round-trip)
    ctx%input_mode = INPUT_MODE_STANDARD
    ctx%has_filename = .true.
    ctx%source_name = "issue_2247_test"

    call transform_with_context(source_code, output_code, error_msg, ctx)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') &
            'FAIL: transform_with_context returned error: ' // trim(error_msg)
        error stop 1
    end if

    ! Check that output contains the assignment "contains = 2.0"
    found_assignment = .false.
    found_print = .false.

    ! Look for "contains = 2.0" or "contains =2.0" etc.
    if (index(output_code, 'contains') > 0) then
        ! More specifically check for the assignment
        if (index(output_code, 'contains = 2.0') > 0 .or. &
            index(output_code, 'contains =2.0') > 0 .or. &
            index(output_code, 'contains= 2.0') > 0 .or. &
            index(output_code, 'contains=2.0') > 0) then
            found_assignment = .true.
        end if
    end if

    ! Look for "print *, contains"
    if (index(output_code, 'print') > 0 .and. &
        index(output_code, 'contains') > 0) then
        ! The print statement should contain 'contains' as the variable
        found_print = .true.
    end if

    ! Report results
    if (.not. found_assignment) then
        write (error_unit, '(A)') &
            'FAIL: assignment "contains = 2.0" not found in output (BUG #2247)'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    if (.not. found_print) then
        write (error_unit, '(A)') &
            'FAIL: print statement with contains not found (BUG #2247)'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') output_code
        error stop 1
    end if

    print *, 'PASS: Issue #2247 test passed - contains identifier preserved'

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

end program test_issue_2247_contains_identifier
