program test_intrinsic_functions_math_expr
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: output_code, error_msg

    print *, "Testing intrinsic functions in mathematical expressions..."

    ! Test the exact code from issue #92
    call read_example('examples/f90/intrinsic_functions_math_expr_complex.f90', &
        test_code)

    call transform_lazy_fortran_string(test_code, output_code, error_msg)

    if (len(error_msg) == 0) then
        print *, "✓ Complex mathematical expression with sqrt compiled successfully"
        print *, "✓ No type mismatch errors occurred"
    else
        print *, "ERROR: Failed to analyze mathematical expression"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if

    ! Test simpler sqrt case
    call read_example('examples/f90/intrinsic_functions_math_expr_simple.f90', &
        test_code)

    call transform_lazy_fortran_string(test_code, output_code, error_msg)

    if (len(error_msg) == 0) then
        print *, "✓ Simple sqrt expression compiled successfully"
    else
        print *, "ERROR: Failed to analyze simple sqrt expression"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if

    ! Test nested sqrt expressions
    call read_example('examples/f90/intrinsic_functions_math_expr_nested.f90', &
        test_code)

    call transform_lazy_fortran_string(test_code, output_code, error_msg)

    if (len(error_msg) == 0) then
        print *, "✓ Nested sqrt expressions compiled successfully"
    else
        print *, "ERROR: Failed to analyze nested sqrt expressions"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if

    print *, "All intrinsic function tests passed!"

contains

    include '../common/read_example.inc'
end program test_intrinsic_functions_math_expr
