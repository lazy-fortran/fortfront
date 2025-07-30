program main
    implicit none
    character(len=2) :: short
    character(len=13) :: medium
    character(len=72) :: long_str
    character(len=5) :: name

    ! Test string type inference
    short = "Hi"
    medium = "Hello, World!"
    long_str = "This is a much longer string that demonstrates proper length inference"
    name = "Alice"
    print *, short
    print *, medium
    print *, long_str
    print *, "Name is:", name
end program main
