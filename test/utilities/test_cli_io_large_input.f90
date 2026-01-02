program test_cli_io_large_input
    use, intrinsic :: iso_fortran_env, only: error_unit
    use test_filesystem_helpers, only: check_if_windows, cleanup_file, &
                                       make_temp_file_path
    implicit none

    character(len=:), allocatable :: text
    integer :: status
    character(len=:), allocatable :: fname
    integer :: u, i
    character(len=10240) :: line
    logical :: is_windows

    is_windows = check_if_windows()
    fname = make_temp_file_path('ff_cli_io_large_', '.txt', is_windows)

    line = repeat('a', len(line))

    open (newunit=u, file=fname, status='replace', action='write')
    do i = 1, 1024
        write (u, '(A)') line
    end do
    close (u)

    call read_all_stdin_or_file(.true., fname, text, status)

    if (status == 4) then
        print *, 'PASS: Large input correctly reported as too large'
    else
        print *, 'FAIL: Expected status=4 for too large input, got', status
        if (allocated(text)) then
            print *, 'INFO: Partial text length =', len(text)
        end if
        stop 1
    end if

    ! Cleanup
    call cleanup_file(fname)

contains

    include '../common/read_example.inc'

end program test_cli_io_large_input
