program test_issue_1785_list_directed_read
    ! Test for issue #1785: READ statements without unit specifier in
    ! parentheses should be preserved (list-directed read)
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer :: x, y, unit, iostat
    character(len=100) :: tempfile
    real(dp) :: a, b

    print *, 'Testing list-directed READ statement preservation'

    ! Create a temporary file with test data
    tempfile = 'test_1785_temp.dat'
    unit = 10

    ! Write test data
    open (unit=unit, file=tempfile, status='replace', iostat=iostat)
    if (iostat /= 0) then
        print *, 'Test failed: Cannot create temp file'
        stop 1
    end if
    write (unit, *) 42, 99
    write (unit, *) 3.14d0, 2.71d0
    close (unit)

    ! Test list-directed READ with file unit
    open (unit=unit, file=tempfile, status='old', iostat=iostat)
    if (iostat /= 0) then
        print *, 'Test failed: Cannot open temp file'
        stop 1
    end if

    read (unit, *) x, y
    if (x /= 42 .or. y /= 99) then
        print *, 'Test failed: Integer read incorrect'
        stop 1
    end if

    read (unit, *) a, b
    if (abs(a - 3.14d0) > 1.0d-6 .or. abs(b - 2.71d0) > 1.0d-6) then
        print *, 'Test failed: Real read incorrect'
        stop 1
    end if

    close (unit)

    ! Clean up
    open (unit=unit, file=tempfile, status='old')
    close (unit, status='delete')

    print *, 'List-directed READ statement test passed!'

end program test_issue_1785_list_directed_read
