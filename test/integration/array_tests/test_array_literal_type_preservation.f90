program test_array_literal_type_preservation
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output, error_msg
    type(format_options_t) :: options
    logical :: test_passed

    test_passed = .true.

    ! Test 1: Integer array literal type preservation
    print *, "Test 1: Integer array [2, 3, 4]"
    call read_example('examples/lf/array_literal_integer.lf', source)
    call transform_lazy_fortran_string_with_format(source, output, error_msg, options)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "  Error: ", trim(error_msg)
        test_passed = .false.
    else
        print *, "  Output:"
        print *, trim(output)

        if (index(output, "integer") > 0) then
            print *, "  PASS: Variable i has integer type"
        else
            print *, "  FAIL: Variable i should have integer type, not real"
            test_passed = .false.
        end if
    end if

    ! Test 2: Real array literal (should stay real)
    print *, ""
    print *, "Test 2: Real array [1.0, 2.5, 3.14]"
    call read_example('examples/lf/array_literal_real.lf', source)
    call transform_lazy_fortran_string_with_format(source, output, error_msg, options)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "  Error: ", trim(error_msg)
        test_passed = .false.
    else
        print *, "  Output:"
        print *, trim(output)

        if (index(output, "real") > 0) then
            print *, "  PASS: Variable x has real type"
        else
            print *, "  FAIL: Variable x should have real type"
            test_passed = .false.
        end if
    end if

    ! Test 3: Mixed array literal (should be real)
    print *, ""
    print *, "Test 3: Mixed array [1, 2.0, 3]"
    call read_example('examples/lf/array_literal_mixed.lf', source)
    call transform_lazy_fortran_string_with_format(source, output, error_msg, options)

    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "  Error: ", trim(error_msg)
        test_passed = .false.
    else
        print *, "  Output:"
        print *, trim(output)

        if (index(output, "real") > 0) then
            print *, "  PASS: Variable y has real type (promoted from mixed)"
        else
            print *, "  FAIL: Variable y should have real type for mixed array"
            test_passed = .false.
        end if
    end if

    if (test_passed) then
        print *, ""
        print *, "All tests passed!"
        stop 0
    else
        print *, ""
        print *, "Some tests failed!"
        stop 1
    end if


contains

    include '../../common/cli_io_reader.inc'

    include '../../common/read_example.inc'
end program test_array_literal_type_preservation
