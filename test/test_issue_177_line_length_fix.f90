program test_issue_177_line_length_fix
    ! Test for issue #177: Line length enforcement with continuations
    use fortfront, only: transform_lazy_fortran_string_with_format, &
                          format_options_t
    implicit none
    
    character(len=:), allocatable :: output, error_msg
    type(format_options_t) :: options
    
    print *, "=== Testing Issue #177: Line Length Enforcement ==="
    
    ! Test case 1: Long assignment should be broken
    call test_line_breaking()
    
    ! Test case 2: Short line should not be broken  
    call test_no_breaking()
    
    print *, "All line length enforcement tests passed"
    
contains
    
    subroutine test_line_breaking()
        character(len=*), parameter :: long_assignment = &
            "result = very_long_variable_name_one + very_long_var_two"
        integer :: continuation_count, ampersand_pos
        
        options%line_length = 50  ! Force breaking
        
        call transform_lazy_fortran_string_with_format(long_assignment, &
                                                     output, error_msg, &
                                                     options)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            stop 1
        end if
        
        ! Count continuation characters
        continuation_count = 0
        ampersand_pos = 1
        do
            ampersand_pos = index(output(ampersand_pos:), " &")
            if (ampersand_pos == 0) exit
            continuation_count = continuation_count + 1
            ampersand_pos = ampersand_pos + 2
        end do
        
        if (continuation_count == 0) then
            print *, "FAIL: No continuation found in long line"
            print *, "Input length: ", len(long_assignment)
            print *, "Output: ", trim(output)
            stop 1
        end if
        
        if (continuation_count > 1) then
            print *, "PASS: Long line broken with ", continuation_count, &
                     " continuations"
        else
            print *, "PASS: Long line broken with continuation"
        end if
    end subroutine test_line_breaking
    
    subroutine test_no_breaking()
        character(len=*), parameter :: short_assignment = "x = y + z"
        
        options%line_length = 80  ! Should not break
        
        call transform_lazy_fortran_string_with_format(short_assignment, &
                                                     output, error_msg, &
                                                     options)
        
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            stop 1
        end if
        
        ! Verify no continuation was added
        if (index(output, " &") > 0) then
            print *, "FAIL: Unnecessary continuation in short line"
            print *, "Output: ", trim(output)
            stop 1
        end if
        
        print *, "PASS: Short line preserved without continuation"
    end subroutine test_no_breaking
    
end program test_issue_177_line_length_fix