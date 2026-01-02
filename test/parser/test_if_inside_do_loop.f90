program test_if_inside_do_loop
    ! Regression test for Issue #1324: ensure IF statements inside DO loops parse
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg
    integer :: inline_if_count

    print *, "=== Testing IF statements inside DO loops (Issue #1324) ==="

    call read_example('examples/f90/issue_1324_if_inside_do_loop.f90', source)

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
    ! Accept both 0.3 and 0.3d0
    if (index(output, 'if (x > 0.3d0)') > 0 .or. index(output, 'if(x>0.3d0)') > 0 .or. &
        index(output, 'if (x > 0.3)') > 0 .or. index(output, 'if(x>0.3)') > 0) then
        inline_if_count = inline_if_count + 1
    end if
    if (inline_if_count < 1) then
        print *, 'ERROR: missing IF (x > 0.3) or IF (x > 0.3d0) inside loop'
        stop 1
    end if

    ! Accept both 0.2 and 0.2d0
    if (index(output, 'if (x > 0.2d0) then') == 0 .and. index(output, 'if(x>0.2d0)then') == 0 .and. &
        index(output, 'if (x > 0.2) then') == 0 .and. index(output, 'if(x>0.2)then') == 0) then
        print *, 'ERROR: nested IF (x > 0.2) or IF (x > 0.2d0) block missing'
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

contains


    include '../common/read_example.inc'
end program test_if_inside_do_loop
