! Issue #2248: Keyworded READ statements should not be deleted
! READ statements with fmt= keyword should parse and round-trip correctly
program issue_2248_read_keyword
    implicit none
    integer :: x, y, z

    ! READ with fmt= keyword parameter
    read (*, fmt='(I5)') x

    ! READ with format= keyword parameter
    read (*, format='(I5)') y

    ! READ with positional format (for comparison)
    read (*, '(I5)') z

    print *, x, y, z
end program issue_2248_read_keyword
