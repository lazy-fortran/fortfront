program test_type_mismatch
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(format_options_t) :: options
    
    ! Test case from issue #96: complex mathematical expressions with intrinsic functions
    input_code = 'program test' // new_line('A') // &
                 '    real :: a = 1.0, b = 2.0, c = 3.0' // new_line('A') // &
                 '    real :: result' // new_line('A') // &
                 '    result = (a + b) * c / (a - b) + sqrt(a**2 + b**2)' // new_line('A') // &
                 'end program test'
    
    print *, "Testing type mismatch fix..."
    print *, "Source code:"
    print *, input_code
    print *, ""
    
    ! Transform with default format options
    call transform_lazy_fortran_string_with_format(input_code, output_code, error_msg, options)
    
    if (len_trim(error_msg) > 0) then
        print *, "ERROR during transformation:", error_msg
        print *, ""
        print *, "FAILURE: Type mismatch error still occurs."
        print *, "Issue #96 is NOT fixed."
        stop 1
    else
        print *, "SUCCESS: Complex mathematical expression processed without type errors!"
        print *, "Output code:"
        print *, output_code
        print *, ""
        print *, "Issue #96 appears to be fixed."
    end if
    
end program test_type_mismatch