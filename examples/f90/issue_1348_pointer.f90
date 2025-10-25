! Minimal reproducer for issue #1348: Pointer assignment operator => becomes = >
program test_pointer
    implicit none
    integer, target :: x
    integer, pointer :: p

    x = 42
    p => x
    print *, p
end program test_pointer
