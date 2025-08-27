! Test if transformation function works for Issue #517
program test_working_517
    use frontend
    implicit none
    
    character(len=*), parameter :: test_code = &
        'type :: person_t' // new_line('a') // &
        '    integer :: age' // new_line('a') // &
        'end type' // new_line('a') // &
        'program main' // new_line('a') // &
        '    print *, "hello"' // new_line('a') // &
        'end program'
        
    character(len=:), allocatable :: result_code, error_msg
    
    print *, "Testing Issue #517 with transformation function"
    print *, ""
    print *, "Input:"
    print *, test_code
    print *, ""
    
    call transform_lazy_fortran_string(test_code, result_code, error_msg)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "Error: ", error_msg
        stop 1
    else
        print *, "Success! Output:"
        print *, result_code
    end if
    
end program test_working_517