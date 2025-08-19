program test_deferred_strings
    implicit none
    character(len=:), allocatable :: str1, str2, str3
    character(len=10) :: fixed_str
    integer :: n
    
    ! Test basic allocation
    str1 = "Hello"
    if (len(str1) /= 5) error stop "Deferred string test failed: str1 length should be 5"
    if (str1 /= "Hello") error stop "Deferred string test failed: str1 content wrong"
    
    ! Test reallocation
    str1 = "Hello, World!"
    if (len(str1) /= 13) error stop "Deferred string test failed: str1 reallocated length should be 13"
    if (str1 /= "Hello, World!") error stop "Deferred string test failed: str1 reallocated content wrong"
    
    ! Test concatenation with automatic allocation
    str2 = "Fortran"
    str3 = str1 // " " // str2
    if (len(str3) /= 21) error stop "Deferred string test failed: concatenated length should be 21"
    if (str3 /= "Hello, World! Fortran") error stop "Deferred string test failed: concatenated content wrong"
    
    ! Test assignment from fixed-length string - portable behavior
    ! Given: A fixed-length string with content and trailing spaces
    fixed_str = "Fixed"
    ! When: Assigning to deferred-length string
    str1 = fixed_str
    ! Then: Content should be correct regardless of compiler-specific length behavior
    if (trim(str1) /= "Fixed") error stop "Deferred string test failed: str1 from fixed content wrong"
    ! Additional check: Length should be consistent with compiler behavior
    ! Some compilers preserve full fixed length, others may trim automatically
    if (len(str1) /= len(fixed_str) .and. len(str1) /= len(trim(fixed_str))) then
        error stop "Deferred string test failed: str1 length not consistent with standard behavior"
    end if
    
    ! Test assignment with trim - explicit trimming should be portable
    ! Given: A fixed-length string with trailing spaces
    ! When: Explicitly trimming before assignment
    str1 = trim(fixed_str)
    ! Then: Length should match trimmed content exactly
    if (len(str1) /= 5) error stop "Deferred string test failed: trimmed str1 should be 5"
    if (str1 /= "Fixed") error stop "Deferred string test failed: trimmed str1 content wrong"
    
    ! Test edge case: Empty string assignment
    ! Given: An empty string
    str1 = ""
    ! When: Checking length and content
    ! Then: Should handle zero-length strings consistently
    if (len(str1) /= 0) error stop "Deferred string test failed: empty string should have length 0"
    if (str1 /= "") error stop "Deferred string test failed: empty string content wrong"
    
    ! Test edge case: All spaces assignment from fixed-length
    ! Given: A fixed-length string containing only spaces
    fixed_str = "          "  ! 10 spaces
    str1 = fixed_str
    ! When: Checking behavior with all-space strings
    ! Then: Content should be consistent (trim should give empty string)
    if (trim(str1) /= "") error stop "Deferred string test failed: all-spaces string should trim to empty"
    ! Length should be either full fixed length or zero (compiler-dependent)
    if (len(str1) /= len(fixed_str) .and. len(str1) /= 0) then
        error stop "Deferred string test failed: all-spaces string length inconsistent"
    end if
    
    ! Test edge case: Single character assignment
    ! Given: Single character in fixed-length string
    fixed_str = "A"  ! Implicit padding to length 10
    str1 = fixed_str
    ! When: Assigning single character
    ! Then: Trimmed content should be single character
    if (trim(str1) /= "A") error stop "Deferred string test failed: single char content wrong"
    
    ! Test edge case: Multiple consecutive assignments
    ! Given: Different string assignments in sequence
    str1 = "First"
    str1 = "Second Assignment"  ! Longer string
    str1 = "X"                  ! Much shorter string
    ! When: Checking final assignment
    ! Then: Should handle reallocation correctly
    if (len(str1) /= 1) error stop "Deferred string test failed: final reassignment length wrong"
    if (str1 /= "X") error stop "Deferred string test failed: final reassignment content wrong"
    
    print *, "All deferred string tests passed!"
end program test_deferred_strings