program test_double_free_complex_code
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: output_code, error_msg
    logical :: success

    print *, "Testing double free issue with complex nested code..."

    ! Test the exact code from issue #88
    call read_example('examples/f90/issue_88_double_free_complex.f90', test_code)

    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    success = (len(error_msg) == 0)

    if (success) then
        print *, "✓ Complex nested code compiled successfully"
        print *, "✓ No double free error occurred"
        print *, "✓ implicit_statement_node handled correctly"
    else
        print *, "ERROR: Compilation failed"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if

    print *, "All tests passed!"

contains

    include '../common/read_example.inc'
end program test_double_free_complex_code
