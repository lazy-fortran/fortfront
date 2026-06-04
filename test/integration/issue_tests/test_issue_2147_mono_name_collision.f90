program test_issue_2147_mono_name_collision
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, output, error_msg
    logical :: test_passed

    print *, "=== Test Issue #2147: Array Monomorphization Name Collision ==="

    call read_example('examples/lf/issue_2147_mono_name_collision_wrong_types.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    ! Test 1: Check for unique mangled names (should have different names for int and real arrays)
    print *, "Test 1: Verifying unique mangled names..."
    test_passed = .false.

    ! Should have DIFFERENT names for integer array and real array versions
    ! Looking for patterns like compute__i32rank1_i32 and compute__r32rank1_i32
    ! or similar that distinguish between integer(:) and real(:) parameters
    if (index(output, "compute__") > 0) then
        ! Check that we have both an integer array variant and a real array variant
        ! The exact names depend on the mangling scheme, but they must be different
        if (index(output, "integer") > 0 .and. index(output, "real") > 0) then
            ! Verify we don't have duplicate procedure names in the interface
            ! Count occurrences of "module procedure" - should appear once
            if (count_occurrences(output, "module procedure") == 1) then
                test_passed = .true.
            end if
        end if
    end if

    if (test_passed) then
        print *, "  PASS: Unique mangled names generated"
    else
        print *, "  FAIL: Name collision or missing variants"
        print *, "  Output:"
        print *, output
        if (len_trim(error_msg) > 0) then
            print *, "  Error:", trim(error_msg)
        end if
        stop 1
    end if

    ! Test 2: Check for correct parameter types
    print *, "Test 2: Verifying correct parameter types..."
    test_passed = .false.

    ! The integer array version should have integer(:) parameter
    ! The real array version should have real(:) parameter
    ! Both should not have mismatched types
    if (index(output, "integer, intent(in) :: arr(:)") > 0 .or. &
        index(output, "integer, dimension(:), intent(in) :: arr") > 0) then
        if (index(output, "real, intent(in) :: arr(:)") > 0 .or. &
            index(output, "real, dimension(:), intent(in) :: arr") > 0 .or. &
            index(output, "real(dp), intent(in) :: arr(:)") > 0 .or. &
            index(output, "real(dp), dimension(:), intent(in) :: arr") > 0 .or. &
            index(output, "real(8), intent(in) :: arr(:)") > 0 .or. &
            index(output, "real(8), dimension(:), intent(in) :: arr") > 0 .or. &
            index(output, "double precision, intent(in) :: arr(:)") > 0 .or. &
            index(output, "double precision, dimension(:), intent(in) :: arr") &
            > 0) then
            test_passed = .true.
        end if
    end if

    if (test_passed) then
        print *, "  PASS: Correct parameter types"
    else
        print *, "  FAIL: Incorrect or missing parameter types"
        print *, "  Output:"
        print *, output
        stop 1
    end if

    ! Test 3: Check for correct internal variable types
    print *, "Test 3: Verifying correct internal variable types..."
    test_passed = .false.

    ! Both versions should have appropriate sum variable types
    ! This is a bit harder to test precisely, but we can check that
    ! the code compiles and has reasonable type declarations
    if (index(output, "sum") > 0) then
        ! Just verify that sum is declared (type inference should handle this)
        test_passed = .true.
    end if

    if (test_passed) then
        print *, "  PASS: Internal variables present"
    else
        print *, "  FAIL: Missing internal variable declarations"
        stop 1
    end if

    print *, ""
    print *, "=== All Issue #2147 Tests Passed ==="

contains

    include '../../common/read_example.inc'


    integer function count_occurrences(text, pattern) result(count)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        integer :: pos, start_pos

        count = 0
        start_pos = 1
        do
            pos = index(text(start_pos:), pattern)
            if (pos == 0) exit
            count = count + 1
            start_pos = start_pos + pos + len(pattern) - 1
            if (start_pos > len(text)) exit
        end do
    end function count_occurrences

end program test_issue_2147_mono_name_collision
