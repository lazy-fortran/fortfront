program test_issue_321_module_with_implicit_main
    ! RED phase test for GitHub Issue #321: For source file with module and main program,
    ! main program does not appear in translated code
    use fortfront
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, "=== Testing Issue #321: Module with implicit main program ==="
    
    ! Run the specific test for issue #321
    if (.not. test_module_with_implicit_main_program()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All Issue #321 tests passed"
        stop 0
    else
        print '(a)', "Some Issue #321 tests failed"
        stop 1
    end if
    
contains

    logical function test_module_with_implicit_main_program()
        ! Given: Source file with module definition followed by implicit main program
        ! When: Code generation processes the mixed source
        ! Then: Both module and main program should appear in translated output
        
        character(len=:), allocatable :: source, output, error_msg
        
        test_module_with_implicit_main_program = .true.
        print *, "Testing module with implicit main program translation..."
        
        ! This is the exact test case from Issue #321
        source = "module m" // new_line('a') // &
                 "contains" // new_line('a') // &
                 "subroutine foo()" // new_line('a') // &
                 'print*,"hi"' // new_line('a') // &
                 "end subroutine" // new_line('a') // &
                 "end module" // new_line('a') // &
                 new_line('a') // &
                 "use m" // new_line('a') // &
                 "call foo()" // new_line('a') // &
                 "end"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR during transformation: ", trim(error_msg)
                test_module_with_implicit_main_program = .false.
                return
            end if
        end if
        
        print *, "Generated output:"
        print *, trim(output)
        print *, ""
        
        ! RED Phase: This test should currently FAIL 
        ! Expected behavior: Both module AND main program should be present in output
        
        ! Check 1: Module should be present (this currently works)
        if (index(output, "module m") == 0) then
            print *, "  FAIL: Module 'm' declaration not found in output"
            test_module_with_implicit_main_program = .false.
        end if
        
        if (index(output, "subroutine foo") == 0) then
            print *, "  FAIL: Subroutine 'foo' not found in module"
            test_module_with_implicit_main_program = .false.
        end if
        
        ! Check 2: Main program should be present (this currently fails - RED phase)
        if (index(output, "use m") == 0) then
            print *, "  FAIL: Main program 'use m' statement not found in output"
            test_module_with_implicit_main_program = .false.
        end if
        
        if (index(output, "call foo") == 0) then
            print *, "  FAIL: Main program 'call foo' statement not found in output"
            test_module_with_implicit_main_program = .false.
        end if
        
        ! Check 3: Main program should have proper program structure
        ! When fixed, we expect either explicit program wrapper or preserved implicit structure
        if (index(output, "program ") > 0) then
            ! If wrapped in explicit program, check for proper structure
            if (index(output, "end program") == 0) then
                print *, "  FAIL: Found program declaration but missing 'end program'"
                test_module_with_implicit_main_program = .false.
            end if
        end if
        
        if (test_module_with_implicit_main_program) then
            print *, "  PASS: All components found in translated output"
        else
            print *, "  EXPECTED FAILURE: Main program components missing (Issue #321)"
            print *, "  This RED phase test documents the current defect"
        end if
        
    end function test_module_with_implicit_main_program

end program test_issue_321_module_with_implicit_main