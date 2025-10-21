program test_cli_stdout_sanitize
    use, intrinsic :: iso_fortran_env, only: error_unit, int64
    use stdout_sanitizer, only: sanitize_file_path
    implicit none

    character(len=256) :: tmpfile
    integer :: rc

    call make_tmpfile(tmpfile)
    call write_polluted_file(trim(tmpfile))
    rc = sanitize_file_path(trim(tmpfile))
    call assert_equal_int(rc, 0, 'sanitize_file_path should succeed on file')
    call assert_file_size(trim(tmpfile), 0_int64)
    call delete_file(trim(tmpfile))

    call make_tmpfile(tmpfile)
    call run_cli_capture(trim(tmpfile))
    call assert_file_not_polluted(trim(tmpfile))

    print *, 'PASS: CLI stdout sanitation removes fpm build noise'
    stop 0

contains

    subroutine make_tmpfile(path)
        character(len=*), intent(out) :: path
        integer :: clock_count
        character(len=256) :: buffer

        call system_clock(count=clock_count)
        write (buffer, '(A,I0,A)') 'build/tmp_cli_sanitize_', clock_count, '.tmp'
        path = trim(buffer)
    end subroutine make_tmpfile

    subroutine write_polluted_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios

        open (newunit=unit, file=path, status='replace', action='write', iostat=ios)
        call assert_equal_int(ios, 0, 'Failed to open polluted file for writing')
        write (unit, '(A)') 'Project is up to date'
        close (unit)
    end subroutine write_polluted_file

    subroutine run_cli_capture(path)
        character(len=*), intent(in) :: path
        character(len=512) :: command
        character(len=256) :: fixture_base
        character(len=512) :: fixture_path
        integer :: exit_status

        call make_tmpfile(fixture_base)
        write (fixture_path, '(A,A)') trim(fixture_base), '_fixture.lf'
        call write_cli_fixture(trim(fixture_path))

        write (command, '(A,A,A,A,A)') 'fpm run fortfront -- "', &
            trim(fixture_path), '" > "', trim(path), '"'
        call execute_command_line(trim(command), exitstat=exit_status)
        call delete_file(trim(fixture_path))
        call assert_equal_int(exit_status, 0, 'fpm run fortfront command failed')
    end subroutine run_cli_capture

    subroutine assert_file_not_polluted(path)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: unit, ios

        open (newunit=unit, file=path, status='old', action='read', iostat=ios)
        call assert_equal_int(ios, 0, 'Failed to open CLI output file')
        read (unit, '(A)', iostat=ios) line
        close (unit, status='delete')
        call assert_equal_int(ios, 0, 'CLI output file was empty')
        if (index(line, 'Project is up to date') /= 0) then
            write (error_unit, '(A)') 'fpm build message leaked into CLI output'
            error stop 1
        end if
    end subroutine assert_file_not_polluted

    subroutine assert_file_size(path, expected_size)
        character(len=*), intent(in) :: path
        integer(int64), intent(in) :: expected_size
        integer(int64) :: actual_size

        inquire (file=path, size=actual_size)
        if (actual_size /= expected_size) then
            write (error_unit, '(A,A,I0,A,I0)') 'Unexpected file size for ', &
                trim(path), ': ', actual_size, ' vs ', expected_size
            error stop 1
        end if
    end subroutine assert_file_size

    subroutine delete_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios

        open (newunit=unit, file=path, status='old', action='readwrite', iostat=ios)
        if (ios == 0) then
            close (unit, status='delete')
        end if
    end subroutine delete_file

    subroutine write_cli_fixture(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios

        open (newunit=unit, file=path, status='replace', action='write', iostat=ios)
        call assert_equal_int(ios, 0, 'Failed to open CLI fixture file for writing')
        write (unit, '(A)') 'module simple_test'
        write (unit, '(A)') '    implicit none'
        write (unit, '(A)') 'contains'
        write (unit, '(A)') '    function square(x)'
        write (unit, '(A)') '        real, intent(in) :: x'
        write (unit, '(A)') '        real :: square'
        write (unit, '(A)') '        square = x * x'
        write (unit, '(A)') '    end function square'
        write (unit, '(A)') 'end module simple_test'
        close (unit)
    end subroutine write_cli_fixture

    subroutine assert_equal_int(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message

        if (actual /= expected) then
            write (error_unit, '(A,A,I0,A,I0)') trim(message), ' (actual=', actual, &
                ', expected=', expected, ')'
            error stop 1
        end if
    end subroutine assert_equal_int

end program test_cli_stdout_sanitize
