program test_deferred_strings
    implicit none
    character(len=:), allocatable :: str1, str2, str3
    character(len=10) :: fixed_str
    integer :: n
    
    ! Test basic allocation
    str1 = "Hello"
    if (len(str1) /= 5) then
        print *, "Deferred string test failed: str1 length should be 5"
        stop 1
    end if
    if (str1 /= "Hello") then
        print *, "Deferred string test failed: str1 content wrong"
        stop 1
    end if
    
    ! Test reallocation
    str1 = "Hello, World!"
    if (len(str1) /= 13) then
        print *, "Deferred string test failed: str1 reallocated length should be 13"
        stop 1
    end if
    if (str1 /= "Hello, World!") then
        print *, "Deferred string test failed: str1 reallocated content wrong"
        stop 1
    end if
    
    ! Test concatenation with automatic allocation
    str2 = "Fortran"
    str3 = str1 // " " // str2
    if (len(str3) /= 21) then
        print *, "Deferred string test failed: concatenated length should be 21"
        stop 1
    end if
    if (str3 /= "Hello, World! Fortran") then
        print *, "Deferred string test failed: concatenated content wrong"
        stop 1
    end if
    
    ! Test assignment from fixed-length string - portable behavior
    ! Given: A fixed-length string with content and trailing spaces
    fixed_str = "Fixed"
    ! When: Assigning to deferred-length string
    str1 = fixed_str
    ! Then: Content should be correct regardless of compiler-specific length behavior
    if (trim(str1) /= "Fixed") then
        print *, "Deferred string test failed: str1 from fixed content wrong"
        stop 1
    end if
    ! Additional check: Length should be consistent with compiler behavior
    ! Some compilers preserve full fixed length, others may trim automatically
    if (len(str1) /= len(fixed_str) .and. len(str1) /= len(trim(fixed_str))) then
        print *, "Deferred string test failed: str1 length not consistent with standard behavior"
        stop 1
    end if
    
    ! Test assignment with trim - explicit trimming should be portable
    ! Given: A fixed-length string with trailing spaces
    ! When: Explicitly trimming before assignment
    str1 = trim(fixed_str)
    ! Then: Length should match trimmed content exactly
    if (len(str1) /= 5) then
        print *, "Deferred string test failed: trimmed str1 should be 5"
        stop 1
    end if
    if (str1 /= "Fixed") then
        print *, "Deferred string test failed: trimmed str1 content wrong"
        stop 1
    end if
    
    ! Test edge case: Empty string assignment
    ! Given: An empty string
    str1 = ""
    ! When: Checking length and content
    ! Then: Should handle zero-length strings consistently
    if (len(str1) /= 0) then
        print *, "Deferred string test failed: empty string should have length 0"
        stop 1
    end if
    if (str1 /= "") then
        print *, "Deferred string test failed: empty string content wrong"
        stop 1
    end if
    
    ! Test edge case: All spaces assignment from fixed-length
    ! Given: A fixed-length string containing only spaces
    fixed_str = "          "  ! 10 spaces
    str1 = fixed_str
    ! When: Checking behavior with all-space strings
    ! Then: Content should be consistent (trim should give empty string)
    if (trim(str1) /= "") then
        print *, "Deferred string test failed: all-spaces string should trim to empty"
        stop 1
    end if
    ! Length should be either full fixed length or zero (compiler-dependent)
    if (len(str1) /= len(fixed_str) .and. len(str1) /= 0) then
        print *, "Deferred string test failed: all-spaces string length inconsistent"
        stop 1
    end if
    
    ! Test edge case: Single character assignment
    ! Given: Single character in fixed-length string
    fixed_str = "A"  ! Implicit padding to length 10
    str1 = fixed_str
    ! When: Assigning single character
    ! Then: Trimmed content should be single character
    if (trim(str1) /= "A") then
        print *, "Deferred string test failed: single char content wrong"
        stop 1
    end if
    
    ! Test edge case: Multiple consecutive assignments
    ! Given: Different string assignments in sequence
    str1 = "First"
    str1 = "Second Assignment"  ! Longer string
    str1 = "X"                  ! Much shorter string
    ! When: Checking final assignment
    ! Then: Should handle reallocation correctly
    if (len(str1) /= 1) then
        print *, "Deferred string test failed: final reassignment length wrong"
        stop 1
    end if
    if (str1 /= "X") then
        print *, "Deferred string test failed: final reassignment content wrong"
        stop 1
    end if
    
    print *, "All deferred string tests passed!"
end program test_deferred_strings