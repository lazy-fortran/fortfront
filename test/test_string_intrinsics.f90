program test_string_intrinsics
    implicit none
    character(len=10) :: str1
    character(len=*), parameter :: str2 = "Hello"
    character(len=:), allocatable :: str3, str4
    character(len=20) :: str5
    integer :: n
    
    ! Test LEN intrinsic
    n = len(str1)
    if (n /= 10) error stop "LEN test failed: str1 length should be 10"
    
    n = len(str2)
    if (n /= 5) error stop "LEN test failed: str2 length should be 5"
    
    ! Test string assignment and concatenation
    str1 = "World"
    str5 = str1 // " " // str2
    ! Note: str1 has padding (10 chars), plus space, then str2
    if (str5(1:5) /= "World") error stop "Concatenation test failed: first part"
    if (str5(12:16) /= "Hello") error stop "Concatenation test failed: second part"
    
    ! Test TRIM intrinsic
    str1 = "Test      "
    str3 = trim(str1)
    if (len(str3) /= 4) error stop "TRIM test failed: trimmed length should be 4"
    if (str3 /= "Test") error stop "TRIM test failed: content wrong"
    
    ! Test LEN_TRIM intrinsic
    n = len_trim(str1)
    if (n /= 4) error stop "LEN_TRIM test failed: should be 4"
    
    ! Test INDEX intrinsic
    str5 = "Hello World"
    n = index(str5, "World")
    if (n /= 7) error stop "INDEX test failed: position should be 7"
    
    n = index(str5, "xyz")
    if (n /= 0) error stop "INDEX test failed: should return 0 for not found"
    
    ! Test ADJUSTL and ADJUSTR
    str1 = "   Test   "
    str3 = adjustl(str1)
    if (str3(1:4) /= "Test") error stop "ADJUSTL test failed"
    
    str4 = adjustr(str1)
    if (str4(7:10) /= "Test") error stop "ADJUSTR test failed"
    
    print *, "All string intrinsic tests passed!"
end program test_string_intrinsics