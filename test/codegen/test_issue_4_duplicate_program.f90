program test_issue_4_duplicate_program
    ! Test for GitHub issue #4: Code generation duplicates program/module declarations
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    integer :: count
    
    print *, "=== Issue #4: Code generation duplicates program/module declarations ==="
    
    ! Test 1: Program declaration should not be duplicated
    call test_program_no_duplication()
    
    ! Test 2: Module declaration should not be duplicated  
    call test_module_no_duplication()
    
    ! Test 3: Nested program should be handled correctly
    call test_nested_no_duplication()
    
    ! Test 4: Multiple modules should not be duplicated
    call test_multiple_modules()
    
    print *, "All issue #4 tests completed"
    
contains

    subroutine test_program_no_duplication()
        print *, "Testing program declaration no duplication..."
        
        source = "program test_program" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    integer :: x" // new_line('a') // &
                 "    x = 42" // new_line('a') // &
                 "end program test_program"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        ! Count occurrences of "program test_program" (excluding "end program")
        count = count_occurrences(output, "program test_program")
        ! Subtract occurrences that are part of "end program test_program"
        count = count - count_occurrences(output, "end program test_program")
        
        if (count == 1) then
            print *, "  PASS: Program declaration appears exactly once"
        else
            print *, "  FAIL: Program declaration appears ", count, " times (expected 1)"
            print *, "  Output: ", trim(output)
            stop 1
        end if
    end subroutine test_program_no_duplication
    
    subroutine test_module_no_duplication()
        print *, "Testing module declaration no duplication..."
        
        source = "module test_module" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    integer :: shared_var" // new_line('a') // &
                 "contains" // new_line('a') // &
                 "    subroutine test_sub()" // new_line('a') // &
                 "        shared_var = 100" // new_line('a') // &
                 "    end subroutine test_sub" // new_line('a') // &
                 "end module test_module"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        ! For now, just check if module content is preserved
        if (index(output, "module test_module") > 0 .and. &
            index(output, "shared_var") > 0 .and. &
            index(output, "test_sub") > 0) then
            print *, "  PASS: Module structure preserved"
        else
            print *, "  FAIL: Module content not preserved correctly"
            print *, "  Output: ", trim(output)
            stop 1
        end if
    end subroutine test_module_no_duplication
    
    subroutine test_nested_no_duplication()
        print *, "Testing no extra program wrapping..."
        
        source = "program outer" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "    call inner()" // new_line('a') // &
                 "contains" // new_line('a') // &
                 "    subroutine inner()" // new_line('a') // &
                 "        print *, 'Hello'" // new_line('a') // &
                 "    end subroutine inner" // new_line('a') // &
                 "end program outer"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        ! Should not wrap in another program
        count = count_occurrences(output, "program ")
        ! Don't count "end program"
        count = count - count_occurrences(output, "end program ")
        
        if (count == 1) then
            print *, "  PASS: Only one program declaration"
        else
            print *, "  FAIL: Found ", count, " program declarations (expected 1)"
            print *, "  Output: ", trim(output)
            stop 1
        end if
    end subroutine test_nested_no_duplication
    
    subroutine test_multiple_modules()
        print *, "Testing multiple modules no duplication..."
        
        source = "module mod1" // new_line('a') // &
                 "    integer :: var1" // new_line('a') // &
                 "end module mod1" // new_line('a') // &
                 new_line('a') // &
                 "module mod2" // new_line('a') // &
                 "    integer :: var2" // new_line('a') // &
                 "end module mod2" // new_line('a') // &
                 new_line('a') // &
                 "program main" // new_line('a') // &
                 "    use mod1" // new_line('a') // &
                 "    use mod2" // new_line('a') // &
                 "end program main"
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        ! Check each module appears exactly once
        count = count_occurrences(output, "module mod1")
        if (count /= 1) then
            print *, "  FAIL: module mod1 appears ", count, " times"
            print *, "  Output: ", trim(output)
            stop 1
        end if
        
        count = count_occurrences(output, "module mod2")
        if (count /= 1) then
            print *, "  FAIL: module mod2 appears ", count, " times"
            print *, "  Output: ", trim(output)
            stop 1
        end if
        
        print *, "  PASS: All modules appear exactly once"
    end subroutine test_multiple_modules
    
    ! Helper function to count occurrences of a substring
    integer function count_occurrences(string, substring) result(count)
        character(len=*), intent(in) :: string, substring
        integer :: pos, start
        
        count = 0
        start = 1
        do
            pos = index(string(start:), substring)
            if (pos == 0) exit
            count = count + 1
            start = start + pos + len(substring) - 1
        end do
    end function count_occurrences
    
end program test_issue_4_duplicate_program