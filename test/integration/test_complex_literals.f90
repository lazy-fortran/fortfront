program test_complex_literals
    use fortfront
    implicit none

    character(len=:), allocatable :: output, error_msg
    logical :: success

    ! Test 1: Basic complex literal
    call transform_lazy_fortran_string("complex :: z = (1.0, 2.0)", output, error_msg)
    success = len_trim(error_msg) == 0
    print *, "Test 1 - Basic complex literal: ", success
    if (.not. success) print *, "Error: ", error_msg
    if (success) print *, "Output: ", trim(output)

    ! Test 2: Complex double precision
    call transform_lazy_fortran_string("complex(kind=8) :: w = (1.0d0, 2.0d0)", output, error_msg)
    success = len_trim(error_msg) == 0
    print *, "Test 2 - Complex double precision: ", success
    if (.not. success) print *, "Error: ", error_msg
    if (success) print *, "Output: ", trim(output)

    ! Test 3: Multiple complex declarations
    call transform_lazy_fortran_string("complex :: a = (1.0, 0.0), b = (0.0, 1.0)", output, error_msg)
    success = len_trim(error_msg) == 0
    print *, "Test 3 - Multiple complex declarations: ", success
    if (.not. success) print *, "Error: ", error_msg
    if (success) print *, "Output: ", trim(output)

    ! Test 4: Complex with arithmetic expressions
    call transform_lazy_fortran_string("complex :: c = (1.0 + 2.0, 3.0 * 4.0)", output, error_msg)
    success = len_trim(error_msg) == 0
    print *, "Test 4 - Complex with expressions: ", success
    if (.not. success) print *, "Error: ", error_msg
    if (success) print *, "Output: ", trim(output)

    ! Test 5: Non-complex type with parentheses (should not be treated as complex)
    call transform_lazy_fortran_string("real :: x = (5.0)", output, error_msg)
    success = len_trim(error_msg) == 0
    print *, "Test 5 - Real with parentheses: ", success
    if (.not. success) print *, "Error: ", error_msg
    if (success) print *, "Output: ", trim(output)

    ! Test 6: Integer with parentheses (should not be treated as complex)
    call transform_lazy_fortran_string("integer :: n = (42)", output, error_msg)
    success = len_trim(error_msg) == 0
    print *, "Test 6 - Integer with parentheses: ", success
    if (.not. success) print *, "Error: ", error_msg
    if (success) print *, "Output: ", trim(output)

end program test_complex_literals
