! Issue #2249: Keyworded WRITE statements must survive parsing
program issue_2249_keyworded_write
    implicit none
    integer :: x, y, z

    ! WRITE with fmt= keyword parameter
    write (*, fmt='(I5)') x

    ! WRITE with format= keyword parameter
    write (*, format='(I5)') y

    ! Positional format spec for comparison
    write (*, '(I5)') z

    print *, x, y, z
end program issue_2249_keyworded_write
