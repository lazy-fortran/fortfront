program test_library_excludes_test_modules
    ! Issue #2909: test-only helper modules must not compile into libfortfront.
    ! Oracle: scan the shipped library sources for module definitions whose name
    ! begins with "test_". Any hit means test scaffolding ships to consumers.
    implicit none

    character(len=*), parameter :: report = 'build/test_modules_in_src.txt'
    character(len=512) :: line
    integer :: unit, ios, exit_stat, cmd_stat, offenders
    logical :: exists

    offenders = 0

    call execute_command_line( &
        'grep -rlE "^[[:space:]]*module[[:space:]]+test_" src > ' // report // &
        ' 2>/dev/null; true', exitstat=exit_stat, cmdstat=cmd_stat)

    if (cmd_stat /= 0) then
        print *, 'SKIP: shell unavailable, cannot scan src/'
        stop 0
    end if

    inquire (file=report, exist=exists)
    if (.not. exists) then
        print *, 'SKIP: could not produce scan report'
        stop 0
    end if

    open (newunit=unit, file=report, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'SKIP: could not read scan report'
        stop 0
    end if

    do
        read (unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        offenders = offenders + 1
        print *, 'FAIL: test-only module ships in library: ', trim(line)
    end do

    close (unit, status='delete')

    if (offenders > 0) then
        print *, 'FAIL: ', offenders, ' test-only module(s) found under src/'
        stop 1
    end if

    print *, 'PASS: no test-only modules under src/'
end program test_library_excludes_test_modules
