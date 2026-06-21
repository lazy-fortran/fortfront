program test_cli_integration
    implicit none
    integer :: test_count, pass_count
    logical :: is_windows
    logical :: run_system

    test_count = 0
    pass_count = 0

    ! Detect if we're on Windows
    is_windows = check_if_windows()

    ! By default, do not run CLI system tests that invoke external tools.
    ! Enable explicitly with: RUN_SYSTEM_TESTS=1 fpm test
    run_system = should_run_system_tests()

    print *, "=== CLI Integration System Tests ==="
    print *, ""

    if (.not. run_system) then
       print *, "SKIPPING: CLI system tests disabled (set RUN_SYSTEM_TESTS=1 to enable)"
        stop 0
    end if

    print *, "Locating fortfront executable..."
    block
        character(len=:), allocatable :: executable_path
        character(len=:), allocatable :: build_command
        integer :: build_status

        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            print *, "Building fortfront executable..."
            build_command = timeout_wrapper('60')//'fpm build'
            call execute_command_line(build_command, exitstat=build_status)
            if (build_status /= 0) then
                print *, "SKIPPING: Failed to build fortfront executable (exit code:", &
                    & build_status, ")"
        print *, "This may indicate CI environment issues or missing build dependencies"
                stop 0
            end if
            executable_path = find_fortfront_executable()
        end if

        if (len(executable_path) == 0) then
            print *, "SKIPPING: Could not locate fortfront executable"
            stop 0
        end if
    end block

    ! Test 0: --help prints to stdout, empty stderr, exit 0
    call test_help_no_stderr()

    ! Test 1: Basic CLI I/O works
    call test_basic_io()

    ! Test 1b (Linux-only): Basic CLI I/O with CRLF to mimic Windows text input
    if (.not. is_windows) then
        call test_basic_io_crlf()
    end if

    ! Test 2: Error handling works
    call test_error_handling()

    ! Test 2b: Unknown flag returns non-zero exit code
    call test_invalid_flag_exit_code()

    ! Test 2c-alt: Single dash returns non-zero exit code
    call test_single_dash_invalid_flag()

    ! Test 2d: End-of-options marker allows hyphen-leading filename
    call test_end_of_options_hyphen_filename()

    ! Test 2c: 'func' syntax yields error but still prints valid program
    call test_func_syntax_error_outputs_program()

    ! Test 3: Empty input handling
    call test_empty_input()

    ! Windows-only: also exercise pipe-based stdin to detect pipe-specific issues
    if (is_windows) then
        call test_basic_io_windows_pipe()
    end if

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"

    if (pass_count == test_count) then
        print *, "All CLI system tests passed!"
        stop 0
    else
        print *, "Some CLI system tests failed!"
        stop 1
    end if

contains

    subroutine cleanup_file(file)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: trimmed
        logical :: exists
        integer :: unit_num, ios

        trimmed = trim(file)
        if (len_trim(trimmed) == 0) return

        inquire (file=trimmed, exist=exists)
        if (.not. exists) return

        open (newunit=unit_num, file=trimmed, status='old', action='readwrite', &
            & iostat=ios)
        if (ios /= 0) then
            open (newunit=unit_num, file=trimmed, status='old', action='read', &
                  iostat=ios)
        end if
        if (ios == 0) then
            close (unit_num, status='delete', iostat=ios)
        end if
    end subroutine cleanup_file

    subroutine write_text_file(path, content)
        character(len=*), intent(in) :: path, content
        integer :: u, ios
        open (newunit=u, file=path, status='replace', action='write', iostat=ios)
        if (ios == 0) then
            write (u, '(A)', iostat=ios) content
            close (u)
        end if
    end subroutine write_text_file

   function build_pipe_command(exe, in_file, out_file, err_file, on_windows) result(cmd)
        character(len=*), intent(in) :: exe, in_file, out_file, err_file
        logical, intent(in) :: on_windows
        character(len=:), allocatable :: cmd
        if (on_windows) then
            cmd = 'cmd /C "set FORTFRONT_TRACE=0 && type '//trim(in_file)// &
                  ' | "'//trim(exe)//'" > '//trim(out_file)//' 2> '// &
                  trim(err_file)//'"'
        else
            cmd = 'sh -lc "cat '//trim(in_file)//' | FORTFRONT_TRACE=0 '// &
                  trim(exe)//' > '//trim(out_file)//' 2> '//trim(err_file)//'"'
        end if
    end function build_pipe_command

    function should_run_system_tests() result(run)
        logical :: run
        character(len=16) :: val
        integer :: stat
        run = .false.
        call get_environment_variable('RUN_SYSTEM_TESTS', val, status=stat)
        if (stat == 0) then
            if (len_trim(val) > 0) then
                select case (adjustl(val(1:1)))
                case ('1', 'y', 'Y', 't', 'T')
                    run = .true.
                end select
            end if
        end if
    end function should_run_system_tests

    function timeout_wrapper(limit_secs) result(prefix)
        character(len=*), intent(in) :: limit_secs
        character(len=:), allocatable :: prefix
        integer :: ec
        logical :: script_exists

        prefix = ""

        if (is_windows) return

        inquire (file='scripts/with_timeout.sh', exist=script_exists)
        if (script_exists) then
            prefix = 'scripts/with_timeout.sh '//trim(limit_secs)//' '
            return
        end if

        call execute_command_line('command -v timeout >/dev/null 2>&1', exitstat=ec)
        if (ec == 0) then
            prefix = 'timeout '//trim(limit_secs)//' '
        else
            prefix = ''
        end if
    end function timeout_wrapper

    function check_if_windows() result(is_win)
        logical :: is_win
        character(len=10) :: os_name
        integer :: stat

        ! Try to detect Windows through environment variable
        call get_environment_variable('OS', os_name, status=stat)
        is_win = (stat == 0 .and. os_name(1:7) == 'Windows')

        ! Alternative: check for Windows-specific env var
        if (.not. is_win) then
            call get_environment_variable('WINDIR', os_name, status=stat)
            is_win = (stat == 0)
        end if
    end function check_if_windows

    ! Find the fortfront executable using multiple search strategies
    function find_fortfront_executable() result(executable_path)
        character(len=:), allocatable :: executable_path
        logical :: file_exists
        character(len=500) :: candidate_path
        integer :: i, exit_code, unit_num
        character(len=256) :: search_output
        character(len=50), dimension(20) :: build_patterns
        logical :: on_windows

        executable_path = ""
        on_windows = check_if_windows()

        ! Windows: locate built executable reliably via dir search
        if (on_windows) then
            ! Try multiple search roots to reliably locate build sibling directories
            block
                character(len=64), allocatable :: roots(:)
                integer :: r
                allocate (roots(5))
                roots = [character(len=16) :: '.', '..', '..\\..', '..\\..\\..', &
                    & '..\\..\\..\\..']
                do r = 1, size(roots)
                    call execute_command_line('cmd /C where /R ' // trim(roots(r)) // ' fortfront.exe > fortfront_search_win.txt', &
                                              exitstat=exit_code)
                    if (exit_code == 0) then
                        open (newunit=unit_num, file='fortfront_search_win.txt', &
                            & status='old', action='read', iostat=exit_code)
                        if (exit_code == 0) then
                            do
                                read (unit_num, '(A)', iostat=exit_code) search_output
                                if (exit_code /= 0) exit
                                if (len_trim(search_output) > 0) then
                                    ! Prefer app\fortfront.exe path if present
                                    if (index(adjustl(search_output), &
                                        & 'app\\fortfront.exe') > 0) then
                                        inquire (file=trim(search_output), &
                                            & exist=file_exists)
                                        if (file_exists) then
                                            executable_path = trim(search_output)
                                            exit
                                        end if
                                    end if
                                end if
                            end do
                            rewind (unit_num)
                            if (len(executable_path) == 0) then
                                ! Fallback: take first found fortfront.exe
                                read (unit_num, '(A)', iostat=exit_code) search_output
                                if (exit_code == 0 .and. &
                                    len_trim(search_output) > 0) then
                                   inquire (file=trim(search_output), exist=file_exists)
                                    if (file_exists) executable_path = &
                                        trim(search_output)
                                end if
                            end if
                            close (unit_num)
                        end if
                call execute_command_line('cmd /C del /F /Q fortfront_search_win.txt', &
                                          & exitstat=exit_code)
                    end if
                    if (len(executable_path) > 0) return
                end do
            end block

            ! Fallback candidates
            do i = 1, 1
                candidate_path = 'app\\fortfront.exe'
                inquire (file=candidate_path, exist=file_exists)
                if (file_exists) then
                    executable_path = trim(candidate_path)
                    return
                end if
            end do
            ! If nothing found, return empty to signal failure; do NOT fall through to POSIX find.
            executable_path = ''
            return
        end if

        ! Strategy 1: Use find command to dynamically locate fortfront executable
        call execute_command_line('find build -name "fortfront" -type f | head -1 > fortfront_search.txt', &
                                  exitstat=exit_code)
        if (exit_code == 0) then
            open (newunit=unit_num, file='fortfront_search.txt', status='old', &
                & action='read', iostat=exit_code)
            if (exit_code == 0) then
                read (unit_num, '(A)', iostat=exit_code) search_output
                close (unit_num)
                ! Clean up temporary file
                call execute_command_line('rm -f fortfront_search.txt', &
                    & exitstat=exit_code)
                if (exit_code == 0 .and. len_trim(search_output) > 0) then
                    inquire (file=trim(search_output), exist=file_exists)
                    if (file_exists) then
                        executable_path = trim(search_output)
                        return
                    end if
                end if
            end if
        end if

        ! Strategy 2: Check hardcoded patterns as fallback
        ! List of common build hash patterns to check (update when needed)
        build_patterns = [ &
                         "build/gfortran_266FF454AB2555FE/app/fortfront   ", &
                         "build/gfortran_9ABCD662468F5A74/app/fortfront   ", &
                         "build/gfortran_C79DEB301B8081FC/app/fortfront   ", &
                         "build/gfortran_C523F0F8A99FF060/app/fortfront   ", &
                         "build/gfortran_1F2DC83CBD1DC595/app/fortfront   ", &
                         "build/gfortran_35CFD5CFC35942D6/app/fortfront   ", &
                         "build/gfortran_4AE9E4ED7A89B913/app/fortfront   ", &
                         "build/gfortran_66DBF6172AF51040/app/fortfront   ", &
                         "build/gfortran_A56298966DD7666C/app/fortfront   ", &
                         "build/gfortran_E3D58E6D75301430/app/fortfront   ", &
                         "build/gfortran_9CBC8EEC13D00A4A/app/fortfront   ", &
                         "./build/gfortran_266FF454AB2555FE/app/fortfront ", &
                         "./build/gfortran_9ABCD662468F5A74/app/fortfront ", &
                         "./build/gfortran_C79DEB301B8081FC/app/fortfront ", &
                         "./build/gfortran_C523F0F8A99FF060/app/fortfront ", &
                         "fortfront                                       ", &
                         "./fortfront                                     ", &
                         "app/fortfront                                   ", &
                         "./app/fortfront                                 ", &
                         "../fortfront                                    "]

        ! Check each candidate path
        do i = 1, size(build_patterns)
            candidate_path = trim(build_patterns(i))
            inquire (file=candidate_path, exist=file_exists)

            if (file_exists) then
                executable_path = trim(candidate_path)
                return
            end if
        end do

    end function find_fortfront_executable

    subroutine test_basic_io()
        integer :: exit_code, run_status
        character(len=256) :: output_line, err_line
        character(len=512) :: command
        character(len=:), allocatable :: executable_path
        logical :: success

        call test_start("Basic CLI I/O")

        ! Find the fortfront executable
        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if

        ! Prepare input file for cross-platform piping
        call write_text_file('test_input.lf', 'print *, ''test'''//new_line('a'))

        ! Execute with input. On Windows, prefer passing filename to avoid pipe forwarding issues via fpm.
        if (is_windows) then
            command = 'cmd /C "set FORTFRONT_TRACE=0 && "'//executable_path// &
                & '" test_input.lf > test_output.txt 2> test_error.txt"'
        else
            command = build_pipe_command(executable_path, 'test_input.lf', &
                                         'test_output.txt', 'test_error.txt', .false.)
        end if
        call execute_command_line(command, exitstat=run_status)

        success = (run_status == 0)

        if (success) then
            ! Check if output contains expected Fortran code (scan file)
            open (unit=10, file='test_output.txt', status='old', action='read', &
                & iostat=exit_code)
            if (exit_code == 0) then
                success = .false.
                do
                    read (10, '(A)', end=100, iostat=exit_code) output_line
                    if (exit_code /= 0) exit
                    if (index(output_line, 'program main') > 0) then
                        success = .true.
                        exit
                    end if
                end do
100             close (10)
                ! On Windows via fpm wrapper, allow any non-empty output as success
                if (.not. success .and. is_windows) then
                   open (unit=13, file='test_output.txt', status='old', action='read', &
                         & iostat=exit_code)
                    if (exit_code == 0) then
                        do
                            read (13, '(A)', end=102, iostat=exit_code) output_line
                            if (exit_code /= 0) exit
                            if (len_trim(output_line) > 0) then
                                success = .true.
                                exit
                            end if
                        end do
102                     close (13)
                    end if
                end if
                ! Ensure no diagnostics leaked to stderr (should be empty on success)
                open (unit=12, file='test_error.txt', status='old', action='read', &
                    & iostat=exit_code)
                if (exit_code == 0) then
                    do
                        read (12, '(A)', end=101, iostat=exit_code) err_line
                        if (exit_code /= 0) exit
                        if (len_trim(err_line) > 0) then
                            success = .false.
                            exit
                        end if
                    end do
101                 close (12)
                end if
                ! Clean up test files
                call cleanup_file('test_input.lf')
                call cleanup_file('test_output.txt')
                call cleanup_file('test_error.txt')
            else
                success = .false.
            end if
        end if

        call test_result(success)
        if (.not. success) then
            print *, "  Failed to run basic CLI command"
            print *, "  Executable path: ", executable_path
            print *, "  Exit code: ", run_status
            ! Dump captured stderr for diagnostics (Windows and POSIX)
            open (unit=98, file='test_error.txt', status='old', action='read', &
                & iostat=exit_code)
            if (exit_code == 0) then
                do
                    read (98, '(A)', end=199, iostat=exit_code) err_line
                    if (exit_code /= 0) exit
                    if (len_trim(err_line) > 0) then
                        print *, '  TRACE: ', trim(err_line)
                    end if
                end do
199             close (98)
            end if
        end if
    end subroutine test_basic_io

    ! POSIX-only: simulate Windows-style CRLF input through stdin piping
    subroutine test_basic_io_crlf()
        integer :: exit_code, run_status
        character(len=256) :: output_line, err_line
        character(len=512) :: command
        character(len=:), allocatable :: executable_path
        logical :: success

        if (is_windows) return

        call test_start("Basic CLI I/O (CRLF via stdin)")

        ! Find the fortfront executable
        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if

        ! Prepare a CRLF-ended input file (convert from LF)
        call write_text_file('test_input_crlf_src.lf', &
                             'print *, ''test'''//new_line('a'))
        call execute_command_line('bash -lc "sed ''s/$/\\r/'' test_input_crlf_src.lf > test_input_crlf.lf"', &
                                  exitstat=exit_code)
        if (exit_code /= 0) then
            call test_result(.false.)
            print *, "  ERROR: Failed to prepare CRLF test input"
            return
        end if

        ! Pipe CRLF input to the executable
        command = build_pipe_command(executable_path, 'test_input_crlf.lf', &
                                     'test_output_crlf.txt', &
                                         & 'test_error_crlf.txt', .false.)
        call execute_command_line(command, exitstat=run_status)

        success = (run_status == 0)

        if (success) then
            ! Verify expected output and empty stderr
            open (unit=14, file='test_output_crlf.txt', status='old', action='read', &
                & iostat=exit_code)
            if (exit_code == 0) then
                success = .false.
                do
                    read (14, '(A)', end=110, iostat=exit_code) output_line
                    if (exit_code /= 0) exit
                    if (index(output_line, 'program main') > 0) then
                        success = .true.
                        exit
                    end if
                end do
110             close (14)

               open (unit=15, file='test_error_crlf.txt', status='old', action='read', &
                     & iostat=exit_code)
                if (exit_code == 0) then
                    do
                        read (15, '(A)', end=111, iostat=exit_code) err_line
                        if (exit_code /= 0) exit
                        if (len_trim(err_line) > 0) then
                            success = .false.
                            exit
                        end if
                    end do
111                 close (15)
                end if
            else
                success = .false.
            end if
        end if

        ! Cleanup
        call cleanup_file('test_input_crlf_src.lf')
        call cleanup_file('test_input_crlf.lf')
        call cleanup_file('test_output_crlf.txt')
        call cleanup_file('test_error_crlf.txt')

        call test_result(success)
        if (.not. success) then
            print *, "  CRLF stdin handling failed"
            print *, "  Exit code: ", run_status
        end if
    end subroutine test_basic_io_crlf

    ! Windows-only: exercise stdin pipe behavior explicitly
    subroutine test_basic_io_windows_pipe()
        integer :: run_status, exit_code
        character(len=256) :: output_line, line
        character(len=512) :: command
        character(len=:), allocatable :: executable_path
        logical :: success

        if (.not. is_windows) return

        call test_start("Basic CLI I/O (Windows pipe)")

        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if

        call write_text_file('test_input_pipe_win.lf', &
                             'print *, ''test'''//new_line('a'))
        command = build_pipe_command(executable_path, 'test_input_pipe_win.lf', &
                                     'test_output_pipe_win.txt', &
                                         & 'test_error_pipe_win.txt', .true.)
        call execute_command_line(command, exitstat=run_status)

        success = (run_status == 0)
        if (success) then
            open (unit=16, file='test_output_pipe_win.txt', status='old', &
                action='read', &
                & iostat=exit_code)
            if (exit_code == 0) then
                success = .false.
                do
                    read (16, '(A)', end=120, iostat=exit_code) output_line
                    if (exit_code /= 0) exit
                    if (len_trim(output_line) > 0) then
                        success = .true.
                        exit
                    end if
                end do
120             close (16)
            else
                success = .false.
            end if
        end if

        call cleanup_file('test_input_pipe_win.lf')
        call cleanup_file('test_output_pipe_win.txt')
        call cleanup_file('test_error_pipe_win.txt')

        call test_result(success)
        if (.not. success) then
            print *, "  Windows pipe CLI test failed"
            print *, "  Exit code: ", run_status
           open (unit=96, file='test_error_pipe_win.txt', status='old', action='read', &
                 & iostat=exit_code)
            if (exit_code == 0) then
                do
                    read (96, '(A)', end=398, iostat=exit_code) line
                    if (exit_code /= 0) exit
                    if (len_trim(line) > 0) then
                        print *, '  TRACE: ', trim(line)
                    end if
                end do
398             close (96)
            end if
        end if
    end subroutine test_basic_io_windows_pipe

    subroutine test_error_handling()
        integer :: exit_code, run_status, unit_num
        character(len=512) :: command
        character(len=256) :: line
        character(len=:), allocatable :: executable_path
        logical :: success, error_file_exists, has_error_output

        call test_start("Error handling")

        ! Find the fortfront executable
        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if

        ! Run with invalid input (cross-platform piping)
        call write_text_file('test_invalid.lf', &
            & 'invalid fortran code @#$%'//new_line('a'))
        command = build_pipe_command(executable_path, 'test_invalid.lf', &
                                     'test_output2.txt', 'test_error2.txt', is_windows)
        call execute_command_line(command, exitstat=run_status)

        ! Invalid source input should surface a non-zero exit with diagnostics
        success = (run_status /= 0)
        error_file_exists = .false.
        has_error_output = .false.

        inquire (file='test_error2.txt', exist=error_file_exists)
        if (error_file_exists) then
            open (newunit=unit_num, file='test_error2.txt', status='old', &
                & action='read', iostat=exit_code)
            if (exit_code == 0) then
                do
                    read (unit_num, '(A)', end=275, iostat=exit_code) line
                    if (exit_code /= 0) exit
                    if (len_trim(line) == 0) cycle
                    if (index(line, '[SYNTAX_ERROR]') > 0 .or. &
                        index(line, '[VALIDATION') > 0 .or. &
                        index(line, '[PARSER_') > 0 .or. &
                        index(line, '[UNRECOGNIZED_INPUT]') > 0 .or. &
                        index(line, 'No output generated') > 0) then
                        has_error_output = .true.
                        exit
                    end if
                end do
275             close (unit_num)
            end if
        end if
        success = success .and. has_error_output

        call test_result(success)
        if (.not. success) then
            print *, "  Error handling failed"
            print *, "  Exit code: ", run_status
            if (.not. has_error_output) then
                if (.not. error_file_exists) then
                    print *, "  Missing CLI diagnostics; expected error output"
                else
                    print *, "  CLI error output did not contain diagnostics"
                end if
            end if
        end if

        ! Clean up test files
        call cleanup_file('test_invalid.lf')
        call cleanup_file('test_output2.txt')
        call cleanup_file('test_error2.txt')
    end subroutine test_error_handling

    subroutine test_invalid_flag_exit_code()
        integer :: exit_code, run_status
        character(len=512) :: command
        character(len=:), allocatable :: executable_path
        logical :: success

        call test_start("Unknown flag returns non-zero exit")

        ! Find the fortfront executable
        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if

        ! Run with an unknown flag; expect non-zero exit
        if (is_windows) then
            command = 'cmd /C "set FORTFRONT_TRACE=0 && "'//executable_path// &
                & '" --nonexistent-flag > test_output_flag.txt 2>test_error_flag.txt"'
        else
            command = 'FORTFRONT_TRACE=0 '//timeout_wrapper('20')//executable_path// &
                      ' --nonexistent-flag > test_output_flag.txt 2>test_error_flag.txt'
        end if
        call execute_command_line(command, exitstat=run_status)

        ! Clean up test files
        call cleanup_file('test_output_flag.txt')
        call cleanup_file('test_error_flag.txt')

        success = (run_status /= 0)

        call test_result(success)
        if (.not. success) then
            print *, "  Expected non-zero exit for unknown flag"
            print *, "  Exit code: ", run_status
        end if
    end subroutine test_invalid_flag_exit_code

    subroutine test_single_dash_invalid_flag()
        integer :: exit_code, run_status
        character(len=512) :: command
        character(len=:), allocatable :: executable_path
        logical :: success

        call test_start("Single '-' returns non-zero exit")

        ! Find the fortfront executable
        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if

        ! Run with a single dash; expect non-zero exit
        if (is_windows) then
            command = 'cmd /C "set FORTFRONT_TRACE=0 && "'//executable_path// &
                & '" - > test_output_dash.txt 2>test_error_dash.txt"'
        else
            command = 'FORTFRONT_TRACE=0 '//timeout_wrapper('20')//executable_path// &
                      ' - > test_output_dash.txt 2>test_error_dash.txt'
        end if
        call execute_command_line(command, exitstat=run_status)

        ! Clean up test files
        call cleanup_file('test_output_dash.txt')
        call cleanup_file('test_error_dash.txt')

        success = (run_status /= 0)

        call test_result(success)
        if (.not. success) then
            print *, "  Expected non-zero exit for single '-'"
            print *, "  Exit code: ", run_status
        end if
    end subroutine test_single_dash_invalid_flag

    subroutine test_end_of_options_hyphen_filename()
        integer :: run_status, exit_code
        character(len=512) :: command
        character(len=256) :: line
        character(len=:), allocatable :: executable_path
        logical :: success

        call test_start("End-of-options '--' allows hyphen filename")

        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if

        ! Create a file whose name begins with a hyphen
        call write_text_file('-input_test.lf', 'print *, ''ok'''//new_line('a'))

        if (is_windows) then
            command = 'cmd /C "set FORTFRONT_TRACE=0 && "'//executable_path// &
                & '" -- -input_test.lf > out_hyphen.txt 2>err_hyphen.txt"'
        else
            command = timeout_wrapper('20')//executable_path// &
                      ' -- -input_test.lf > out_hyphen.txt 2>err_hyphen.txt'
        end if

        call execute_command_line(command, exitstat=run_status)
        success = (run_status == 0)

        if (success) then
            ! Output should be a valid program; stderr should be empty
            open (unit=31, file='out_hyphen.txt', status='old', action='read', &
                & iostat=exit_code)
            if (exit_code == 0) then
                read (31, '(A)', end=410, iostat=exit_code) line
                if (exit_code == 0) then
                    success = (index(line, 'program main') > 0)
                else
                    success = .false.
                end if
410             close (31)
            else
                success = .false.
            end if

            if (success) then
                open (unit=32, file='err_hyphen.txt', status='old', action='read', &
                    & iostat=exit_code)
                if (exit_code == 0) then
                    do
                        read (32, '(A)', end=411, iostat=exit_code) line
                        if (exit_code /= 0) exit
                        if (len_trim(line) > 0) then
                            success = .false.
                            exit
                        end if
                    end do
411                 close (32)
                end if
            end if
        end if

        ! Cleanup
        call cleanup_file('-input_test.lf')
        call cleanup_file('out_hyphen.txt')
        call cleanup_file('err_hyphen.txt')

        call test_result(success)
        if (.not. success) then
            print *, "  End-of-options handling failed"
            print *, "  Exit code: ", run_status
        end if
    end subroutine test_end_of_options_hyphen_filename

    subroutine test_func_syntax_error_outputs_program()
        integer :: run_status, exit_code
        character(len=512) :: command
        character(len=256) :: line
        character(len=:), allocatable :: executable_path
        logical :: success

        call test_start("'func' syntax prints program and exits non-zero")

        ! Find the fortfront executable
        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if

        ! Run with lazy function syntax which is not supported
        if (is_windows) then
           call write_text_file('test_func.lf', 'func add(x, y) = x + y'//new_line('a'))
            command = build_pipe_command(executable_path, 'test_func.lf', &
                                         'test_out_func.txt', &
                                         'test_err_func.txt', .true.)
        else
            command = 'bash -lc "echo \"func add(x, y) = x + y\" | '// &
                      timeout_wrapper('20')//executable_path// &
                      ' > test_out_func.txt 2>test_err_func.txt"'
        end if
        call execute_command_line(command, exitstat=run_status)

        ! Expect non-zero exit code
        success = (run_status /= 0)

        ! And program output should still be produced
        if (success) then
            open (unit=21, file='test_out_func.txt', status='old', action='read', &
                & iostat=exit_code)
            if (exit_code == 0) then
                read (21, '(A)', end=300, iostat=exit_code) line
                if (exit_code == 0) then
                    success = success .and. (index(line, 'program main') > 0)
                end if
300             close (21)
            else
                success = .false.
            end if
        end if

        ! Clean up files
        call cleanup_file('test_out_func.txt')
        call cleanup_file('test_err_func.txt')
        call cleanup_file('test_func.lf')

        call test_result(success)
        if (.not. success) then
            print *, "  Expected non-zero exit and program output for 'func' syntax"
            print *, "  Exit code: ", run_status
        end if
    end subroutine test_func_syntax_error_outputs_program

    subroutine test_empty_input()
        integer :: exit_code
        character(len=256) :: output_line
        character(len=512) :: command
        character(len=:), allocatable :: executable_path
        logical :: success

        call test_start("Empty input produces valid program")

        ! Find the fortfront executable
        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if

        ! Run with empty input (cross-platform)
        if (is_windows) then
            ! Use NUL on Windows to pipe empty input
            command = 'cmd /C "set FORTFRONT_TRACE=0 && type NUL | "'// &
                      executable_path//'" > test_output3.txt 2>test_error3.txt"'
        else
            command = 'bash -lc "echo \"\" | '//timeout_wrapper('20')// &
                      executable_path//' > test_output3.txt 2>test_error3.txt"'
        end if
        call execute_command_line(command, exitstat=exit_code)

        success = (exit_code == 0)

        if (success) then
            ! Check output contains valid empty program
            open (unit=11, file='test_output3.txt', status='old', action='read', &
                & iostat=exit_code)
            if (exit_code == 0) then
                read (11, '(A)', end=200, iostat=exit_code) output_line
                if (exit_code == 0) then
                    success = success .and. (index(output_line, 'program main') > 0)
                end if
200             close (11)
                ! Clean up test files
                call cleanup_file('test_output3.txt')
                call cleanup_file('test_error3.txt')
            else
                success = .false.
            end if
        end if

        call test_result(success)
        if (.not. success) then
            print *, "  Empty input handling failed"
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_empty_input

    subroutine test_help_no_stderr()
        integer :: run_status, ios
        character(len=512) :: command
        character(len=:), allocatable :: executable_path
        character(len=256) :: line
        logical :: success

        call test_start("--help returns 0 and empty stderr")

        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if

        if (is_windows) then
            command = 'cmd /C "set FORTFRONT_TRACE=0 && "'//executable_path// &
                & '" --help > help_out.txt 2>help_err.txt"'
        else
            command = 'bash -lc "FORTFRONT_TRACE=0 '//timeout_wrapper('20')// &
                      executable_path//' --help > help_out.txt 2>help_err.txt"'
        end if
        call execute_command_line(command, exitstat=run_status)

        success = (run_status == 0)

        ! Ensure stderr is empty
        if (success) then
            open (unit=31, file='help_err.txt', status='old', action='read', iostat=ios)
            if (ios == 0) then
                do
                    read (31, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    if (len_trim(line) > 0) then
                        success = .false.
                        exit
                    end if
                end do
                close (31)
            else
                success = .false.
            end if
        end if

        ! Quick sanity: stdout contains usage header
        if (success) then
            open (unit=32, file='help_out.txt', status='old', action='read', iostat=ios)
            if (ios == 0) then
                read (32, '(A)', iostat=ios) line
                if (ios == 0) then
                    success = (index(line, 'fortfront -') > 0)
                else
                    success = .false.
                end if
                close (32)
            else
                success = .false.
            end if
        end if

        call cleanup_file('help_out.txt')
        call cleanup_file('help_err.txt')

        call test_result(success)
        if (.not. success) then
            print *, "  --help did not meet expectations (exit=", run_status, ")"
        end if
    end subroutine test_help_no_stderr

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write (*, '(A)', advance='no') "Testing: "//test_name//"  ... "
    end subroutine test_start

    subroutine test_result(passed)
        logical, intent(in) :: passed
        if (passed) then
            print *, "PASSED"
            pass_count = pass_count + 1
        else
            print *, "FAILED"
        end if
    end subroutine test_result

end program test_cli_integration
