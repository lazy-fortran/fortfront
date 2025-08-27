program debug_issue_517
    use frontend
    implicit none
    
    character(len=:), allocatable :: test_code, result_code, error_msg
    
    ! Test code from Issue #511/517
    test_code = &
        'type :: person_t' // new_line('a') // &
        '    character(len=20) :: name' // new_line('a') // &
        'end type person_t' // new_line('a') // &
        '' // new_line('a') // &
        'program main' // new_line('a') // &
        '    type(person_t) :: p' // new_line('a') // &
        '    p%name = "John"' // new_line('a') // &
        '    print *, p%name' // new_line('a') // &
        'end program main'
    
    print *, "=== Testing Issue #517: Multi-unit parsing ==="
    print *, ""
    print *, "Input code:"
    print *, test_code
    print *, ""
    
    ! Transform the source to see what happens
    call transform_lazy_fortran_string(test_code, result_code, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, "Error: ", trim(error_msg)
        stop 1
    end if
    
    print *, ""
    print *, "Generated code:"
    print *, result_code
    print *, ""
    
    ! The problem should be visible in the generated code structure
    
end program debug_issue_517