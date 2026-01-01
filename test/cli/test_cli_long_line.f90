program test_cli_long_line
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    implicit none

    character(len=:), allocatable :: tmpfile, text
    integer :: unit, ios, status
    integer, parameter :: N = 5000
    character(len=N) :: line
    character(len=N + 2) :: file_content

    call make_tmpfile(tmpfile)

    line = repeat('x', N)
    file_content = line // char(13) // char(10)
    open (newunit=unit, file=tmpfile, status='replace', action='write', &
          access='stream', form='unformatted', iostat=ios)
    if (ios /= 0) then
        write (error_unit, '(A)') 'Failed to open temp file for writing'
        stop 1
    end if
    write (unit) file_content
    close (unit)

    call read_all_stdin_or_file(.true., tmpfile, text, status)
    if (status /= 0) then
        write (error_unit, '(A)') 'Failed to read temp file via CLI reader'
        stop 1
    end if

    if (len(text) /= N + 1) then
        write (error_unit, '(A,I0,A,I0)') 'Length mismatch: got ', len(text), &
            ' expected ', N + 1
        stop 1
    end if

    if (text(1:N) /= line) then
        write (error_unit, '(A)') 'Content mismatch in first N characters'
        stop 1
    end if

    if (text(N + 1:N + 1) /= new_line('A')) then
        write (error_unit, '(A)') 'Missing trailing newline'
        stop 1
    end if

    call cleanup_tmpfile(tmpfile)
    print *, 'PASS: CLI reads long single line without truncation'
    stop 0

contains

    include '../common/cli_io_reader.inc'

    subroutine make_tmpfile(path)
        character(len=:), allocatable, intent(out) :: path
        character(len=256) :: envtmp
        integer :: ios

        ! Prefer POSIX TMPDIR; fall back to Windows TEMP/TMP; finally current dir
        call get_environment_variable('TMPDIR', envtmp, status=ios)
        if (ios /= 0 .or. len_trim(envtmp) == 0) then
            call get_environment_variable('TEMP', envtmp, status=ios)
        end if
        if (ios /= 0 .or. len_trim(envtmp) == 0) then
            call get_environment_variable('TMP', envtmp, status=ios)
        end if
        if (ios /= 0 .or. len_trim(envtmp) == 0) then
            envtmp = '.'
        end if

        path = trim(envtmp) // '/ff_long_line_test_' // to_str(int(1000000 * &
                                                                   rand())) // '.txt'
    end subroutine make_tmpfile

    subroutine cleanup_tmpfile(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        ! Best-effort delete; ignore failures
        open (newunit=unit, file=path, status='old', action='readwrite', iostat=ios)
        if (ios == 0) then
            close (unit, status='delete')
        end if
    end subroutine cleanup_tmpfile

    function to_str(i) result(s)
        integer, intent(in) :: i
        character(len=:), allocatable :: s
        character(len=64) :: buf
        write (buf, '(I0)') i
        s = trim(buf)
    end function to_str

    real function rand()
        integer, save :: seed = 1234567
        seed = mod(seed * 1103515245 + 12345, huge(1))
        rand = real(seed) / real(huge(1))
    end function rand

end program test_cli_long_line
