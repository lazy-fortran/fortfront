program test_final_issue_320
    !! Final test for Issue #320 after comprehensive fixes
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    if (.not. test_duplicate_declarations()) all_passed = .false.
    if (.not. test_type_conflicts()) all_passed = .false.
    if (.not. test_explicit_override()) all_passed = .false.
    
    if (all_passed) then
        print '(a)', "All Issue #320 tests passed - duplication fixed!"
    else
        print '(a)', "Some Issue #320 tests still failing"
        error stop 1
    end if

contains

    logical function test_duplicate_declarations()
        character(len=:), allocatable :: input, output, error_msg
        integer :: x_count, intent_count
        
        test_duplicate_declarations = .true.
        print '(a)', "Testing duplicate declarations fix..."
        
        input = "function twice(x) result(y)" // new_line('A') // &
                "y = 2*x" // new_line('A') // &
                "end function"
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (error_msg /= "") then
            print '(a)', "Error: " // error_msg
            test_duplicate_declarations = .false.
            return
        end if
        
        x_count = count_substring(output, ":: x")
        intent_count = count_substring(output, "intent(in) :: x")
        
        if (x_count <= 1 .and. intent_count <= 1) then
            print '(a)', "PASS: No duplicate declarations"
        else
            print '(a)', "FAIL: Still has duplicates"
            print '(a)', "Output:"
            print '(a)', output
            test_duplicate_declarations = .false.
        end if
        
    end function test_duplicate_declarations
    
    logical function test_type_conflicts()
        character(len=:), allocatable :: input, output, error_msg
        
        test_type_conflicts = .true.
        print '(a)', "Testing type conflicts fix..."
        
        input = "function calc(i) result(j)" // new_line('A') // &
                "j = i + 1" // new_line('A') // &
                "end function"
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (error_msg /= "") then
            print '(a)', "Error: " // error_msg
            test_type_conflicts = .false.
            return
        end if
        
        ! Check for conflicting type declarations
        if (count_substring(output, "integer") > 0 .and. count_substring(output, "real") > 0) then
            if (count_substring(output, ":: i") > 1) then
                print '(a)', "FAIL: Type conflicts still exist"
                print '(a)', "Output:"
                print '(a)', output
                test_type_conflicts = .false.
                return
            end if
        end if
        
        print '(a)', "PASS: No type conflicts"
        
    end function test_type_conflicts
    
    logical function test_explicit_override()
        character(len=:), allocatable :: input, output, error_msg
        
        test_explicit_override = .true.
        print '(a)', "Testing explicit declaration preservation..."
        
        input = "function process(x, y) result(z)" // new_line('A') // &
                "    integer :: y" // new_line('A') // &
                "    z = x + y" // new_line('A') // &
                "end function"
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (error_msg /= "") then
            print '(a)', "Error: " // error_msg
            test_explicit_override = .false.
            return
        end if
        
        ! Check if explicit declaration is preserved without extra intent
        if (index(output, "integer :: y") > 0 .and. count_substring(output, ":: y") == 1) then
            print '(a)', "PASS: Explicit declaration preserved"
        else
            print '(a)', "FAIL: Explicit declaration not preserved correctly"
            print '(a)', "Output:"
            print '(a)', output
            test_explicit_override = .false.
        end if
        
    end function test_explicit_override
    
    integer function count_substring(string, substring)
        character(len=*), intent(in) :: string, substring
        integer :: pos, start
        
        count_substring = 0
        start = 1
        
        do
            pos = index(string(start:), substring)
            if (pos == 0) exit
            count_substring = count_substring + 1
            start = start + pos + len(substring) - 1
        end do
        
    end function count_substring

end program test_final_issue_320