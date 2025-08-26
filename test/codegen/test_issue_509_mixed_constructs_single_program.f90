program test_issue_509_mixed_constructs_single_program
    use frontend
    implicit none
    
    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: result_code
    character(len=:), allocatable :: expected_code
    character(len=:), allocatable :: error_msg
    
    ! Test code from Issue #509
    test_code = &
        'module m' // new_line('a') // &
        'contains' // new_line('a') // &
        'subroutine foo()' // new_line('a') // &
        'print*,"foo"' // new_line('a') // &
        'end subroutine' // new_line('a') // &
        '' // new_line('a') // &
        'subroutine bar()' // new_line('a') // &
        'print*,"bar"' // new_line('a') // &
        'end subroutine bar' // new_line('a') // &
        '' // new_line('a') // &
        'function twice(x) result(y)' // new_line('a') // &
        'y = 2*x' // new_line('a') // &
        'end function' // new_line('a') // &
        '' // new_line('a') // &
        'function thrice(x) result(y)' // new_line('a') // &
        'y = 3*x' // new_line('a') // &
        'end function thrice' // new_line('a') // &
        '' // new_line('a') // &
        'end module'
    
    ! Expected output from Issue #509
    expected_code = &
        'module m' // new_line('a') // &
        'contains' // new_line('a') // &
        'subroutine foo()' // new_line('a') // &
        '    print *, "foo"' // new_line('a') // &
        'end subroutine foo' // new_line('a') // &
        'subroutine bar()' // new_line('a') // &
        '    print *, "bar"' // new_line('a') // &
        'end subroutine bar' // new_line('a') // &
        'function twice(x)' // new_line('a') // &
        '    y = 2*x' // new_line('a') // &
        'end function twice' // new_line('a') // &
        'function thrice(x)' // new_line('a') // &
        '    y = 3*x' // new_line('a') // &
        'end function thrice' // new_line('a') // &
        'end module m'
    
    print *, "Test Issue #509: subroutine and function indentation should be consistent"
    print *, "================================================================"
    
    ! Transform the test code  
    call transform_lazy_fortran_string(test_code, result_code, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, "ERROR: ", error_msg
        stop 1
    end if
    
    print *, ""
    print *, "Input Code:"
    print *, "----------"
    call print_with_line_numbers(test_code)
    
    print *, ""
    print *, "Generated Code:"
    print *, "--------------"
    call print_with_line_numbers(result_code)
    
    print *, ""
    print *, "Expected Code:"
    print *, "-------------"  
    call print_with_line_numbers(expected_code)
    
    print *, ""
    print *, "Analysis:"
    print *, "--------"
    print *, "Current issue: subroutine/function declarations and end statements"
    print *, "should have matching indentation levels"
    
contains
    
    subroutine print_with_line_numbers(code)
        character(len=*), intent(in) :: code
        integer :: i, line_num, start_pos
        character(len=:), allocatable :: line
        
        line_num = 1
        start_pos = 1
        
        do i = 1, len(code)
            if (code(i:i) == new_line('a')) then
                if (i >= start_pos) then
                    line = code(start_pos:i-1)
                    write(*, '(I3,A,A)') line_num, ': ', line
                else
                    write(*, '(I3,A)') line_num, ': '
                end if
                line_num = line_num + 1
                start_pos = i + 1
            end if
        end do
        
        ! Handle last line if no trailing newline
        if (start_pos <= len(code)) then
            line = code(start_pos:)
            write(*, '(I3,A,A)') line_num, ': ', line
        end if
    end subroutine print_with_line_numbers
    
end program test_issue_509_mixed_constructs_single_program