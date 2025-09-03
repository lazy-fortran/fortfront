! Test for issue #926: Module function/subroutine bodies missing from generated code  
program test_module_contains_simple
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    
    ! Test module with function
    input_code = "module test_mod" // new_line('a') // &
                 "contains" // new_line('a') // &
                 "function add(a, b) result(c)" // new_line('a') // &
                 "integer :: a, b, c" // new_line('a') // &
                 "c = a + b" // new_line('a') // &
                 "end function add" // new_line('a') // &
                 "end module test_mod"
    
    print *, "Input:"
    print *, trim(input_code)
    print *, ""
    
    ! Transform the code
    call transform_lazy_fortran_string(input_code, output_code, error_msg)
    
    if (allocated(error_msg)) then
        print *, "Error: ", trim(error_msg)
        error stop 1
    end if
    
    print *, "Output:"
    print *, trim(output_code)
    print *, ""
    
    ! Check that the contains section and function are preserved
    if (index(output_code, 'contains') == 0) then
        print *, "FAIL: 'contains' keyword missing from output"
        error stop 1
    end if
    
    if (index(output_code, 'function add') == 0 .and. &
        index(output_code, 'function add(') == 0) then
        print *, "FAIL: 'function add' missing from output"
        error stop 1
    end if
    
    if (index(output_code, 'c = a + b') == 0) then
        print *, "FAIL: Function body 'c = a + b' missing from output"
        error stop 1
    end if
    
    print *, "[PASS] test_module_contains_simple"
    
end program test_module_contains_simple