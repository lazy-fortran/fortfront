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
    
    ! Test assignment from fixed-length string
    fixed_str = "Fixed"
    str1 = fixed_str
    if (len(str1) /= 10) error stop "Deferred string test failed: str1 from fixed should be 10"
    if (str1(1:5) /= "Fixed") error stop "Deferred string test failed: str1 from fixed content wrong"
    
    ! Test assignment with trim
    str1 = trim(fixed_str)
    if (len(str1) /= 5) error stop "Deferred string test failed: trimmed str1 should be 5"
    if (str1 /= "Fixed") error stop "Deferred string test failed: trimmed str1 content wrong"
    
    print *, "All deferred string tests passed!"
end program test_deferred_strings