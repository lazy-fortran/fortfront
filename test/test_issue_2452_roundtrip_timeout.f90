program test_issue_2452_roundtrip_timeout
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, iostat_end, iostat_eor, input_unit
    implicit none
    character(len=:), allocatable :: source, output1, output2, error_msg
    logical :: success
    print *, "=== Testing Issue #2452: Roundtrip Timeout ==="
    print *

    ! Read the example file with complex array intrinsics
    call read_example('examples/f90/issue_2452_roundtrip_timeout.f90', source)

    ! First pass: parse -> emit
    print *, "First pass: parsing original source..."
    call transform_lazy_fortran_string(source, output1, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: First pass failed: ' // error_msg
        stop 1
    end if

    if (.not. allocated(output1) .or. len_trim(output1) == 0) then
        write (error_unit, '(A)') 'FAIL: First pass produced no output'
        stop 1
    end if

    print *, "First pass: SUCCESS"

    ! Second pass: parse emitted output -> emit again
    ! This is where the hang would occur before the fix
    print *, "Second pass: parsing emitted output..."
    call transform_lazy_fortran_string(output1, output2, error_msg)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: Second pass failed: ' // error_msg
        stop 1
    end if

    if (.not. allocated(output2) .or. len_trim(output2) == 0) then
        write (error_unit, '(A)') 'FAIL: Second pass produced no output'
        stop 1
    end if

    print *, "Second pass: SUCCESS"

    ! Verify that output contains expected constructs
    success = .true.
    success = success .and. verify_contains(output2, 'lbound', &
        'lbound intrinsic preserved')
    success = success .and. verify_contains(output2, 'ubound', &
        'ubound intrinsic preserved')
    success = success .and. verify_contains(output2, 'spread', &
        'spread intrinsic preserved')
    success = success .and. verify_contains(output2, 'maxloc', &
        'maxloc intrinsic preserved')
    success = success .and. verify_contains(output2, 'minloc', &
        'minloc intrinsic preserved')
    success = success .and. verify_contains(output2, 'reshape', &
        'reshape intrinsic preserved')

    if (.not. success) then
        write (error_unit, '(A)') 'FAIL: Output verification failed'
        stop 1
    end if

    print *, "PASS: All roundtrip timeout tests passed"
    print *, "      First pass completed successfully"
    print *, "      Second pass completed successfully (no hang)"
    print *, "      All intrinsics preserved in output"

contains

    include 'common/cli_io_reader.inc'
    include 'common/read_example.inc'

    function verify_contains(text, pattern, description) result(found)
        character(len=*), intent(in) :: text, pattern, description
        logical :: found

        found = index(text, pattern) > 0
        if (found) then
            print *, "  PASS: " // trim(description)
        else
            write (error_unit, '(A)') '  FAIL: ' // trim(description)
            write (error_unit, '(A)') '        Expected pattern: ' // trim(pattern)
        end if
    end function verify_contains



end program test_issue_2452_roundtrip_timeout
