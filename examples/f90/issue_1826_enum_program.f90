! Issue #1826: enum construct inside a program
program test_enum
    enum, bind(c)
    enumerator :: RED = 1
    enumerator :: GREEN = 2
    end enum
    integer :: color
    color = RED
    print *, color
end program test_enum
