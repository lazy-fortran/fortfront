program test_io_statements
    implicit none
    integer :: unit, iostat, i
    character(len=100) :: line
    character(len=20) :: filename
    real :: data(5)
    
    ! Test data
    data = [1.0, 2.0, 3.0, 4.0, 5.0]
    filename = "test_data.txt"
    unit = 10
    
    ! Test OPEN and WRITE statements
    open(unit=unit, file=filename, status="replace", iostat=iostat)
    if (iostat /= 0) then
        error stop "I/O test failed: Cannot open file for writing"
    end if
    
    ! Write some data
    write(unit, '(A)') "Test data file"
    write(unit, '(5F10.3)') data
    write(unit, '(A,I5)') "Count:", size(data)
    
    close(unit)
    
    ! Test READ statement
    open(unit=unit, file=filename, status="old", iostat=iostat)
    if (iostat /= 0) then
        error stop "I/O test failed: Cannot open file for reading"
    end if
    
    read(unit, '(A)') line
    if (trim(line) /= "Test data file") then
        error stop "I/O test failed: First line read incorrectly"
    end if
    
    ! Read the data back
    read(unit, *) data
    
    ! Check the data
    do i = 1, 5
        if (abs(data(i) - real(i)) > 1e-6) then
            error stop "I/O test failed: Data read incorrectly"
        end if
    end do
    
    close(unit)
    
    ! Clean up test file
    open(unit=unit, file=filename, status="old")
    close(unit, status="delete")
    
    print *, "All I/O statement tests passed!"
    
end program test_io_statements