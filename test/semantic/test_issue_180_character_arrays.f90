program test_issue_180_character_arrays
    ! Test for GitHub issue #180: Character arrays translated as real
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Issue #180: Character arrays translated as real ==="
    
    ! Test the exact case from the issue
    call test_character_array_assignment()
    
    print *, "Issue #180 test completed"
    
contains

    subroutine test_character_array_assignment()
        print *, "Testing character array assignment..."
        
        source = 'program test' // new_line('a') // &
                 's = ["mom", "dad"]' // new_line('a') // &
                 'print*,s' // new_line('a') // &
                 'end program test'
        
        call transform_lazy_fortran_string(source, output, error_msg)
        
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if
        
        print *, "INPUT:"
        print *, trim(source)
        print *, ""
        print *, "OUTPUT:"
        print *, trim(output)
        print *, ""
        
        ! Check that output does NOT contain "real" for character arrays
        if (index(output, "real") > 0) then
            print *, "  FAIL: Found 'real' type for character array"
            print *, "  This indicates the character array was incorrectly typed as real"
            stop 1
        end if
        
        ! Check that output contains proper character declaration
        if (index(output, "character") == 0) then
            print *, "  FAIL: Missing 'character' type declaration"
            print *, "  Character arrays should be declared with character type"
            stop 1
        end if
        
        print *, "  PASS: Character arrays correctly typed as character, not real"
        
    end subroutine test_character_array_assignment
    
end program test_issue_180_character_arrays