! Issue #2248: Keyworded READ statements should not be deleted
! READ statements with fmt= keyword should parse and round-trip correctly
program issue_2248_read_keyword
    implicit none
    integer :: x, y

    ! READ with fmt= keyword parameter
    read (*, fmt = '(I5)') x

    ! READ with positional format (for comparison)
    read (*, '(I5)') y

    print *, x, y
end program issue_2248_read_keyword
