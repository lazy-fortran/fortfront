program test_cli_io_large_input
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    implicit none

    character(len=:), allocatable :: text
    integer :: status
    character(len=*), parameter :: fname = 'tmp_large_input.txt'
    integer :: u, i
    character(len=10240) :: line

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
    open (newunit=u, file=fname, status='old', action='read')
    close (u, status='delete')

contains

    include '../common/read_example.inc'

end program test_cli_io_large_input
