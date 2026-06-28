program test_cli_preprocessed_roundtrip
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    character(len=256) :: tmp_in
    character(len=256) :: tmp_out
    character(len=512) :: exe_path
    character(len=1024) :: command
    integer :: exit_status
    logical :: found

    ! Skip this test unless RUN_SYSTEM_TESTS=1 is set
    if (.not. should_run_system_tests()) then
        print *, 'SKIP: CLI preprocessed roundtrip test (set RUN_SYSTEM_TESTS=1)'
        stop 0
    end if

    call find_executable(exe_path, found)
    if (.not. found) then
        write (error_unit, '(A)') 'ERROR: Could not find fortfront executable'
        stop 1
    end if

    call make_tmpfile(tmp_in)
    call make_tmpfile(tmp_out)
    call write_preprocessed_snippet(trim(tmp_in))

    command = trim(exe_path) // ' ' // trim(tmp_in) // ' > ' // trim(tmp_out)
    call execute_command_line(trim(command), exitstat=exit_status)
    ! Preprocessed line directives are currently not supported; the CLI must
    ! fail loudly instead of silently accepting the input. We only require
    ! a non-zero exit status here, not specific stdout contents.
    if (exit_status == 0) then
        write (error_unit, '(A)') 'ERROR: CLI unexpectedly succeeded on preprocessed input'
        call delete_file(trim(tmp_in))
        call delete_file(trim(tmp_out))
        stop 1
    end if

    call delete_file(trim(tmp_in))
    call delete_file(trim(tmp_out))

    print *, 'PASS: CLI rejects unsupported preprocessed line directives without hiding errors'
    stop 0

contains

    include '../common/cli_system_tests.inc'

    subroutine write_preprocessed_snippet(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios

        open (newunit=unit, file=path, status='replace', action='write', &
            iostat=ios)
        call assert_equal_int(ios, 0, 'Failed to open preprocessed snippet file')
        write (unit, '(A)') '# 1 "fake_line.F"'
        write (unit, '(A)') '! simple preprocessed snippet'
        write (unit, '(A)') '# 2 "fake_line.F"'
        write (unit, '(A)') '      parameter (k = 2)'
        write (unit, '(A)') '      end'
        close (unit)
    end subroutine write_preprocessed_snippet

end program test_cli_preprocessed_roundtrip
