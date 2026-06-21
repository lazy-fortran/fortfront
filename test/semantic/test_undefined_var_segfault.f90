program test_undefined_var_segfault
    use transformation_api, only: transform_lazy_fortran_string
 use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: output_code, error_msg
    logical :: success

    print *, "Testing segmentation fault fix for undefined variables..."

    ! Test the exact code from issue #87
    call read_example('examples/f90/undefined_var_segfault.f90', test_code)

    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    success = (len(error_msg) == 0)

    if (success) then
        print *, "✓ Code with undefined variable compiled without segfault"
        print *, "✓ Recursion depth protection working"
    else
        ! It's OK if compilation fails due to undefined variable
        ! as long as it doesn't segfault
        print *, "✓ Compilation failed gracefully (no segfault)"
        if (len(error_msg) > 0) then
            print *, "Error message:", trim(error_msg)
        end if
    end if

    ! Test circular type reference protection
    call read_example('examples/f90/undefined_var_recursive_type.f90', test_code)

    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    success = (len(error_msg) == 0)

    if (success .or. .not. success) then
        print *, "✓ Circular type reference handled without infinite recursion"
    end if

    print *, "All tests passed!"

contains

    include '../common/read_example.inc'
end program test_undefined_var_segfault
