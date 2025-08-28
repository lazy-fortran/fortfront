program test_string_intrinsics
    implicit none
    character(len=10) :: str1
    character(len=*), parameter :: str2 = "Hello"
    character(len=:), allocatable :: str3, str4
    character(len=20) :: str5
    integer :: n
    
    ! Test LEN intrinsic
    n = len(str1)
    if (n /= 10) then
        print *, "LEN test failed: str1 length should be 10"
        stop 1
    end if
    
    n = len(str2)
    if (n /= 5) then
        print *, "LEN test failed: str2 length should be 5"
        stop 1
    end if
    
    ! Test string assignment and concatenation
    str1 = "World"
    str5 = str1 // " " // str2
    ! Note: str1 has padding (10 chars), plus space, then str2
    if (str5(1:5) /= "World") then
        print *, "Concatenation test failed: first part"
        stop 1
    end if
    if (str5(12:16) /= "Hello") then
        print *, "Concatenation test failed: second part"
        stop 1
    end if
    
    ! Test TRIM intrinsic
    str1 = "Test      "
    str3 = trim(str1)
    if (len(str3) /= 4) then
        print *, "TRIM test failed: trimmed length should be 4"
        stop 1
    end if
    if (str3 /= "Test") then
        print *, "TRIM test failed: content wrong"
        stop 1
    end if
    
    ! Test LEN_TRIM intrinsic
    n = len_trim(str1)
    if (n /= 4) then
        print *, "LEN_TRIM test failed: should be 4"
        stop 1
    end if
    
    ! Test INDEX intrinsic
    str5 = "Hello World"
    n = index(str5, "World")
    if (n /= 7) then
        print *, "INDEX test failed: position should be 7"
        stop 1
    end if
    
    n = index(str5, "xyz")
    if (n /= 0) then
        print *, "INDEX test failed: should return 0 for not found"
        stop 1
    end if
    
    ! Test ADJUSTL and ADJUSTR
    str1 = "   Test   "
    str3 = adjustl(str1)
    if (str3(1:4) /= "Test") then
        print *, "ADJUSTL test failed"
        stop 1
    end if
    
    str4 = adjustr(str1)
    if (str4(7:10) /= "Test") then
        print *, "ADJUSTR test failed"
        stop 1
    end if
    
    print *, "All string intrinsic tests passed!"
end program test_string_intrinsics