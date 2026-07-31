program test_char_len
    implicit none
    character(len=10) :: str1
    character(len=*), parameter :: str2 = "Hello"
    character*20 :: str3
    character(len=12) :: text

    str1 = "Test"
    str3 = "Old style"
    print *, str1, str2, str3

end program test_char_len
