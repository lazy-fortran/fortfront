program test_pause_statement
    implicit none
    integer :: x

    x = 42
    print *, "Before pause:", x

    pause "Press Enter to continue..."

    x = 99
    print *, "After pause:", x
end program test_pause_statement
