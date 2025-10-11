program test_if_inside_do_loop
    ! Regression test for Issue #1324: ensure IF statements inside DO loops parse
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg
    integer :: inline_if_count

    print *, "=== Testing IF statements inside DO loops (Issue #1324) ==="

    source = "program inline_if_fixture" // new_line('a') // &
             "  implicit none" // new_line('a') // &
             "  real :: x" // new_line('a') // &
             "  integer :: i, j, n" // new_line('a') // &
             "  n = 3" // new_line('a') // &
             "  call random_number(x)" // new_line('a') // &
             "  do i = 1, n" // new_line('a') // &
             "    call random_number(x)" // new_line('a') // &
             '    print*, "x =", x' // new_line('a') // &
             '    if (x > 0.3) print*, "x larger than 0.3"' // new_line('a') // &
             "    if (x > 0.2) then" // new_line('a') // &
             '      print*, "x larger than 0.2"' // new_line('a') // &
             "    end if" // new_line('a') // &
             "    do j = 1, 2" // new_line('a') // &
             '      if (j == 1) print*, "nested iteration", j' // new_line('a') // &
             "    end do" // new_line('a') // &
             "  end do" // new_line('a') // &
             "end program inline_if_fixture"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, '! Unparsed') > 0) then
        print *, 'ERROR: unexpected ! Unparsed placeholder emitted'
        stop 1
    end if

    if (index(output, 'do i = 1, n') == 0 .and. index(output, 'do i=1,n') == 0) then
        print *, 'ERROR: DO loop missing from output'
        stop 1
    end if

    inline_if_count = 0
    if (index(output, 'if (x > 0.3d0)') > 0 .or. index(output, 'if(x>0.3d0)') > 0) then
        inline_if_count = inline_if_count + 1
    end if
    if (inline_if_count < 1) then
        print *, 'ERROR: missing IF (x > 0.3d0) inside loop'
        stop 1
    end if

    if (index(output, 'if (x > 0.2d0) then') == 0 .and. index(output, 'if(x>0.2d0)then') == 0) then
        print *, 'ERROR: nested IF (x > 0.2d0) block missing'
        stop 1
    end if

    if (index(output, 'do j = 1, 2') == 0 .and. index(output, 'do j=1,2') == 0) then
        print *, 'ERROR: nested DO j loop missing from output'
        stop 1
    end if

    if (index(output, 'if (j == 1)') == 0 .and. index(output, 'if(j==1)') == 0) then
        print *, 'ERROR: inner IF on nested loop missing from output'
        stop 1
    end if

    print *, 'PASS: parser retains control flow inside DO loops without placeholders'
    stop 0
end program test_if_inside_do_loop
