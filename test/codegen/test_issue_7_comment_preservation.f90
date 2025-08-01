program test_issue_7_comment_preservation
    ! Test for GitHub issue #7: Comments are not preserved in emit_fortran output
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Issue #7: Comments are not preserved in emit_fortran output ==="
    
    ! Test with inline and block comments
    source = "! Program to test comment handling" // new_line('a') // &
             "x = 42          ! Initialize x" // new_line('a') // &
             "y = x*2       ! Double it" // new_line('a') // &
             "print *, ""x ="", x      ! Print x"
    
    call transform_lazy_fortran_string(source, output, error_msg)
    
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            stop 1
        end if
    end if
    
    print *, "Input source:"
    print *, trim(source)
    print *
    print *, "Output:"
    print *, trim(output)
    print *
    
    ! Check if comments are preserved
    if (index(output, "! Program to test comment handling") > 0 .and. &
        index(output, "! Initialize x") > 0 .and. &
        index(output, "! Double it") > 0 .and. &
        index(output, "! Print x") > 0) then
        print *, "PASS: All comments preserved in output"
    else
        print *, "FAIL: Comments not preserved in output"
        stop 1
    end if
    
end program test_issue_7_comment_preservation