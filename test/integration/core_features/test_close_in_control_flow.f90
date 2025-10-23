program test_close_in_control_flow
    implicit none
    integer :: unit, iostat, i, choice
    character(len=30) :: filename

    unit = 10

    ! Test 1: CLOSE in simple IF block
    filename = "test_if_close.txt"
    open (unit=unit, file=filename, status="replace", iostat=iostat)
    if (iostat == 0) then
        write (unit, '(A)') "CLOSE in IF block"
        close (unit)
    end if

    ! Test 2: CLOSE in nested IF blocks
    filename = "test_nested_if.txt"
    open (unit=unit, file=filename, status="replace", iostat=iostat)
    if (iostat == 0) then
        if (.true.) then
            write (unit, '(A)') "CLOSE in nested IF"
            close (unit)
        end if
    end if

    ! Test 3: CLOSE in DO loop
    do i = 1, 2
        write (filename, '(A,I1,A)') "test_do_", i, ".txt"
        open (unit=unit, file=filename, status="replace", iostat=iostat)
        if (iostat == 0) then
            write (unit, '(A,I1)') "File ", i
            close (unit)
        end if
    end do

    ! Test 4: CLOSE in SELECT CASE
    choice = 1
    select case (choice)
    case (1)
        filename = "test_case1.txt"
        open (unit=unit, file=filename, status="replace", iostat=iostat)
        if (iostat == 0) then
            write (unit, '(A)') "CLOSE in CASE 1"
            close (unit)
        end if
    case (2)
        filename = "test_case2.txt"
        open (unit=unit, file=filename, status="replace", iostat=iostat)
        if (iostat == 0) then
            write (unit, '(A)') "CLOSE in CASE 2"
            close (unit)
        end if
    end select

    ! Test 5: Multiple CLOSE statements in same block
    filename = "test_multi_close.txt"
    open (unit=unit, file=filename, status="replace", iostat=iostat)
    if (iostat == 0) then
        write (unit, '(A)') "Multiple operations"
        close (unit)
        open (unit=unit, file=filename, status="old", iostat=iostat)
        if (iostat == 0) then
            close (unit)
        end if
    end if

    print *, "All control flow CLOSE tests passed!"

end program test_close_in_control_flow
