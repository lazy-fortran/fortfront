program test_all_examples
    use iso_fortran_env, only: error_unit
    use executable_finder, only: find_fortfront_executable
    implicit none

    integer :: test_count, pass_count, fail_count, skip_count
    integer :: xfail_count, xpass_count
    logical :: is_windows
    character(len=256) :: examples_dir
    character(len=256), allocatable :: expected_failures(:)
    integer :: num_expected_failures
    integer :: ios
    character(len=:), allocatable :: fortfront_exe

    test_count = 0
    pass_count = 0
    fail_count = 0
    skip_count = 0
    xfail_count = 0
    xpass_count = 0

    is_windows = check_if_windows()

    ! Find fortfront executable (avoid fpm run overhead)
    fortfront_exe = find_fortfront_executable()
    if (len(fortfront_exe) == 0) then
        print *, "ERROR: Could not locate fortfront executable"
        print *, "Please run 'fpm build' before running tests"
        stop 1
    end if

    ! fpm test runs from project root, so examples/ is directly accessible
    examples_dir = 'examples'

    ! Load expected failures list
    call load_expected_failures('examples/expected_failures.txt', &
                                expected_failures, num_expected_failures)

    print *, "=== Fortfront Examples Integration Test ==="
    print *, ""
    print *, "Testing all example files (.lf and .f90) in examples/ directory"
    print *, "This validates that documented examples work correctly"
    print *, ""

    ! Test .lf (lazy fortran) examples
    call test_examples_by_extension(examples_dir, '.lf', fortfront_exe, &
                                    test_count, pass_count, fail_count, skip_count, &
                                    xfail_count, xpass_count, is_windows, &
                                    expected_failures, num_expected_failures)

    ! Test .f90 (standard fortran) examples
    call test_examples_by_extension(examples_dir, '.f90', fortfront_exe, &
                                    test_count, pass_count, fail_count, skip_count, &
                                    xfail_count, xpass_count, is_windows, &
                                    expected_failures, num_expected_failures)

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0)') "Total examples tested: ", test_count
    write (*, '(A,I0)') "Passed: ", pass_count
    write (*, '(A,I0)') "Failed: ", fail_count
    write (*, '(A,I0)') "XFail (expected): ", xfail_count
    write (*, '(A,I0)') "XPass (unexpected): ", xpass_count
    write (*, '(A,I0)') "Skipped: ", skip_count

    if (test_count > 0) then
        write (*, '(A,F5.1,A)') "Success rate: ", &
            real(pass_count + xfail_count) * 100.0 / real(test_count), "%"
    end if

    print *, ""

    if (xpass_count > 0) then
        print *, "NOTICE: Some expected failures now pass!"
        print *, "Please update examples/expected_failures.txt"
    end if

    print *, ""

    if (fail_count > 0) then
        print *, "FAILURE: Some examples did not transform correctly"
        print *, "This indicates parser/semantic/codegen issues"
        print *, "See individual test output above for details"
        stop 1
    else if (xpass_count > 0) then
        print *, "FAILURE: Unexpected passes detected"
        print *, "Update examples/expected_failures.txt to remove fixed examples"
        stop 1
    else if (test_count == 0) then
        print *, "WARNING: No examples were tested"
        print *, "Examples directory may not exist or be accessible"
        stop 0
    else
        print *, "SUCCESS: All examples behaved as expected"
        stop 0
    end if

contains

    function check_if_windows() result(is_win)
        logical :: is_win
        character(len=10) :: os_name
        integer :: stat

        call get_environment_variable('OS', os_name, status=stat)
        is_win = (stat == 0 .and. os_name(1:7) == 'Windows')

        if (.not. is_win) then
            call get_environment_variable('WINDIR', os_name, status=stat)
            is_win = (stat == 0)
        end if
    end function check_if_windows

    subroutine cleanup_file(file)
        character(len=*), intent(in) :: file
        integer :: ec
        logical :: is_win
        character(len=:), allocatable :: quoted

        is_win = check_if_windows()
        quoted = quote_for_shell(file, is_win)
        if (len_trim(quoted) == 0) return

        if (is_win) then
            call execute_command_line('cmd /C if exist '//trim(quoted)// &
                                      ' del /F /Q '//trim(quoted), exitstat=ec)
        else
            call execute_command_line('rm -f '//trim(quoted), exitstat=ec)
        end if
    end subroutine cleanup_file

    subroutine load_expected_failures(filename, failures, num_failures)
        character(len=*), intent(in) :: filename
        character(len=256), allocatable, intent(out) :: failures(:)
        integer, intent(out) :: num_failures
        integer :: unit_num, ios, count, i
        character(len=256) :: line, trimmed_line
        logical :: file_exists

        num_failures = 0

        inquire (file=trim(filename), exist=file_exists)
        if (.not. file_exists) then
            allocate (failures(0))
            return
        end if

        ! First pass: count non-empty, non-comment lines
        open (newunit=unit_num, file=trim(filename), status='old', &
              action='read', iostat=ios)
        if (ios /= 0) then
            allocate (failures(0))
            return
        end if

        count = 0
        do
            read (unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            trimmed_line = adjustl(line)
            if (len_trim(trimmed_line) > 0 .and. trimmed_line(1:1) /= '#') then
                count = count + 1
            end if
        end do
        close (unit_num)

        ! Allocate array
        allocate (failures(count))
        num_failures = count

        ! Second pass: read filenames
        open (newunit=unit_num, file=trim(filename), status='old', &
              action='read', iostat=ios)
        i = 0
        do
            read (unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            trimmed_line = adjustl(line)
            if (len_trim(trimmed_line) > 0 .and. trimmed_line(1:1) /= '#') then
                i = i + 1
                ! Extract filename before any # comment
                if (index(trimmed_line, '#') > 0) then
                    failures(i) = adjustl(trimmed_line(1:index(trimmed_line, '#') - 1))
                else
                    failures(i) = trim(trimmed_line)
                end if
            end if
        end do
        close (unit_num)
    end subroutine load_expected_failures

    function is_expected_failure(basename, expected_failures, num_expected_failures) &
        result(is_xfail)
        character(len=*), intent(in) :: basename
        character(len=256), intent(in) :: expected_failures(:)
        integer, intent(in) :: num_expected_failures
        logical :: is_xfail
        integer :: i

        is_xfail = .false.
        do i = 1, num_expected_failures
            if (trim(basename) == trim(expected_failures(i))) then
                is_xfail = .true.
                return
            end if
        end do
    end function is_expected_failure

    subroutine test_examples_by_extension(examples_dir, extension, fortfront_exe, &
                                          test_count, pass_count, fail_count, &
                                              & skip_count, &
                                          xfail_count, xpass_count, is_windows, &
                                          expected_failures, num_expected_failures)
        character(len=*), intent(in) :: examples_dir, extension, fortfront_exe
        integer, intent(inout) :: test_count, pass_count, fail_count, skip_count
        integer, intent(inout) :: xfail_count, xpass_count
        logical, intent(in) :: is_windows
        character(len=256), intent(in) :: expected_failures(:)
        integer, intent(in) :: num_expected_failures

        character(len=500) :: list_command, list_file
        integer :: unit_num, ios
        character(len=256) :: line

        ! Create unique temp file name for this extension
        list_file = 'examples_list' // trim(extension) // '.txt'

        ! List files with this extension
        if (is_windows) then
            list_command = 'cmd /C "dir /B ' // trim(examples_dir) // '\*' // &
                           trim(extension) // ' > ' // trim(list_file) // ' 2>nul"'
        else
            list_command = 'ls ' // trim(examples_dir) // '/*' // &
                           trim(extension) // ' > ' // trim(list_file) // &
                               & ' 2>/dev/null || true'
        end if

        call execute_command_line(trim(list_command), exitstat=ios)

        ! Read and test each example file
        open (newunit=unit_num, file=trim(list_file), status='old', &
              action='read', iostat=ios)

        if (ios /= 0) then
            ! No files found with this extension, that's ok
            call cleanup_file(list_file)
            return
        end if

        do
            read (unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit

            if (len_trim(line) == 0) cycle

            ! Build full path
            if (is_windows) then
                call test_single_example(trim(examples_dir)//'\'//trim(line), &
                    & fortfront_exe, &
                                         test_count, pass_count, fail_count, skip_count, &
                                         xfail_count, xpass_count, is_windows, &
                                         expected_failures, num_expected_failures)
            else
                ! On Unix, ls gives full path already
                call test_single_example(trim(line), fortfront_exe, &
                                         test_count, pass_count, fail_count, skip_count, &
                                         xfail_count, xpass_count, is_windows, &
                                         expected_failures, num_expected_failures)
            end if
        end do

        close (unit_num)
        call cleanup_file(list_file)

    end subroutine test_examples_by_extension

    subroutine test_single_example(filepath, fortfront_exe, test_count, pass_count, &
                                   fail_count, skip_count, xfail_count, xpass_count, &
                                   is_windows, expected_failures, num_expected_failures)
        character(len=*), intent(in) :: filepath, fortfront_exe
        integer, intent(inout) :: test_count, pass_count, fail_count, skip_count
        integer, intent(inout) :: xfail_count, xpass_count
        logical, intent(in) :: is_windows
        character(len=256), intent(in) :: expected_failures(:)
        integer, intent(in) :: num_expected_failures

        character(len=1024) :: command
        character(len=256) :: output_file, error_file, basename_str
        integer :: exit_code, i, unit_out
        logical :: has_error, has_unparsed, file_exists, has_warning, expect_fail
        character(len=512) :: line
        character(len=:), allocatable :: module_dir

        ! Extract basename for display and output files
        i = max(index(filepath, '/', back=.true.), index(filepath, '\', back=.true.))
        if (i > 0) then
            basename_str = filepath(i + 1:)
        else
            basename_str = filepath
        end if

        output_file = 'test_example_' // trim(basename_str) // '_output.f90'
        error_file = 'test_example_' // trim(basename_str) // '.err'

        test_count = test_count + 1

        write (*, '(A)', advance='no') "Testing " // trim(basename_str) // " ... "

        ! Check if example file exists
        inquire (file=trim(filepath), exist=file_exists)
        if (.not. file_exists) then
            print *, "SKIP (file not found)"
            skip_count = skip_count + 1
            return
        end if

        module_dir = get_module_directory(fortfront_exe)

        ! Run fortfront on the example using direct binary (much faster than fpm run)
        ! Note: Errors go to stderr, actual fortran code to stdout
        if (is_windows) then
            command = 'cmd /C "type ' // trim(filepath) // ' | "' // &
                      trim(fortfront_exe) // '" > ' // trim(output_file) // ' 2>' // &
                      trim(error_file) // '"'
        else
            command = 'sh -c "cat ' // trim(filepath) // ' | ' // &
                      trim(fortfront_exe) // ' > ' // trim(output_file) // ' 2>' // &
                      trim(error_file) // '"'
        end if

        call execute_command_line(trim(command), exitstat=exit_code)

        ! Check for errors in output and stderr
        has_error = .false.
        has_unparsed = .false.
        has_warning = .false.

        ! Check stderr file for error markers (parser errors go to stderr)
        open (newunit=unit_out, file=trim(error_file), status='old', &
              action='read', iostat=exit_code)
        if (exit_code == 0) then
            do
                read (unit_out, '(A)', iostat=exit_code) line
                if (exit_code /= 0) exit

                ! Check for errors in stderr
                if (index(line, 'ERROR') > 0) then
                    has_error = .true.
                end if

                ! Check for warnings
                if (index(line, 'WARNING') > 0) then
                    has_warning = .true.
                end if
            end do
            close (unit_out)
        end if

        ! Check output file for code generation issues
        open (newunit=unit_out, file=trim(output_file), status='old', &
              action='read', iostat=exit_code)
        if (exit_code == 0) then
            do
                read (unit_out, '(A)', iostat=exit_code) line
                if (exit_code /= 0) exit

                ! Check for compilation failure marker
                if (index(line, '! COMPILATION FAILED') > 0) then
                    has_error = .true.
                    exit
                end if

                ! Check for unparsed content (indicates incomplete parsing)
                if (index(line, '! Unparsed:') > 0) then
                    has_unparsed = .true.
                end if
            end do
            close (unit_out)
        else
            has_error = .true.
        end if

        ! Try to compile the output with gfortran to catch silent bugs
        if (.not. has_error .and. .not. has_unparsed) then
            command = build_compile_command(output_file, module_dir, is_windows)
            if (len_trim(command) == 0) then
                has_error = .true.
            else
                call execute_command_line(trim(command), exitstat=exit_code)
                if (exit_code /= 0) then
                    has_error = .true.
                end if
            end if
        end if

        ! Check if this test is expected to fail
        expect_fail = is_expected_failure(basename_str, expected_failures, &
                                          num_expected_failures)

        ! Report result
        if (has_error .or. has_unparsed) then
            if (expect_fail) then
                print *, "XFAIL (expected failure)"
                xfail_count = xfail_count + 1
            else
                call report_example_failure(trim(basename_str), output_file, error_file)
                if (has_error) then
                    print *, "FAIL (parser error or compilation failed)"
                else
                    print *, "FAIL (unparsed content - incomplete transformation)"
                end if
                fail_count = fail_count + 1
            end if
        else if (has_warning) then
            if (expect_fail) then
                print *, "XPASS (expected to fail but only has warnings)"
                xpass_count = xpass_count + 1
            else
                print *, "WARN (transformed with warnings)"
                pass_count = pass_count + 1
            end if
        else
            if (expect_fail) then
                print *, "XPASS (expected to fail but passed!)"
                xpass_count = xpass_count + 1
            else
                print *, "PASS"
                pass_count = pass_count + 1
            end if
        end if

        ! Clean up temp files
        call cleanup_file(output_file)
        call cleanup_file(error_file)

    end subroutine test_single_example

    subroutine report_example_failure(name, output_file, error_file)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: output_file
        character(len=*), intent(in) :: error_file
        character(len=256) :: line
        integer :: unit_num, ios, printed
        logical :: exists

        inquire (file=trim(error_file), exist=exists)
        if (exists) then
            print *, "---- stderr for ", trim(name)
            open (newunit=unit_num, file=trim(error_file), status='old', &
                  action='read', iostat=ios)
            if (ios == 0) then
                printed = 0
                do
                    read (unit_num, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    print *, trim(line)
                    printed = printed + 1
                    if (printed >= 10) then
                        exit
                    end if
                end do
                close (unit_num)
            end if
        else
            print *, "---- stderr missing for ", trim(name)
        end if

        inquire (file=trim(output_file), exist=exists)
        if (exists) then
            print *, "---- generated output preview for ", trim(name)
            open (newunit=unit_num, file=trim(output_file), status='old', &
                  action='read', iostat=ios)
            if (ios == 0) then
                printed = 0
                do
                    read (unit_num, '(A)', iostat=ios) line
                    if (ios /= 0) exit
                    print *, trim(line)
                    printed = printed + 1
                    if (printed >= 10) exit
                end do
                close (unit_num)
            end if
        else
            print *, "---- generated output missing for ", trim(name)
        end if
    end subroutine report_example_failure

    function get_module_directory(executable_path) result(module_dir)
        character(len=*), intent(in) :: executable_path
        character(len=:), allocatable :: module_dir
        character(len=:), allocatable :: candidate
        character(len=:), allocatable :: search_file
        character(len=:), allocatable :: command
        character(len=64) :: clock_string
        integer :: clock_count, exit_code, unit_num
        character(len=512) :: path_line
        logical :: is_win

        module_dir = ''

        is_win = check_if_windows()

        call system_clock(count=clock_count)
        write (clock_string, '(I0)') abs(clock_count)
        search_file = 'fortfront_module_search_' // trim(clock_string) // '.txt'

        command = build_module_search_command(search_file, is_win)
        if (len_trim(command) > 0) then
            call execute_command_line(trim(command), exitstat=exit_code)
            if (exit_code == 0) then
                open (newunit=unit_num, file=trim(search_file), status='old', &
                      action='read', iostat=exit_code)
                if (exit_code == 0) then
                    read (unit_num, '(A)', iostat=exit_code) path_line
                    close (unit_num)
                    if (exit_code == 0) then
                        module_dir = directory_from_module_path(trim(path_line))
                        if (len_trim(module_dir) > 0) then
                            call cleanup_file(search_file)
                            return
                        end if
                    end if
                else
                    close (unit_num, status='delete')
                end if
            end if
        end if

        call cleanup_file(search_file)

        candidate = extract_module_candidate(executable_path, '/app/')
        if (len_trim(candidate) > 0) then
            if (module_directory_has_module(candidate, '/')) then
                module_dir = candidate
                return
            end if
        end if

        candidate = extract_module_candidate(executable_path, '\app\')
        if (len_trim(candidate) > 0) then
            if (module_directory_has_module(candidate, '\')) then
                module_dir = candidate
            end if
        end if
    end function get_module_directory

    function build_module_search_command(search_file, is_windows) result(command)
        character(len=*), intent(in) :: search_file
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: command

        command = ''

        if (.not. is_safe_path(search_file)) return

        if (is_windows) then
            command = 'cmd /C "dir /B /S fortfront.mod > ' // trim(search_file) // '"'
        else
            command = 'find build -name "fortfront.mod" -type f | head -1 > ' // &
                      trim(search_file)
        end if
    end function build_module_search_command

    function directory_from_module_path(path_line) result(directory)
        character(len=*), intent(in) :: path_line
        character(len=:), allocatable :: directory
        integer :: pos

        directory = ''
        if (len_trim(path_line) == 0) return

        pos = index(path_line, '/fortfront.mod', back=.true.)
        if (pos > 0) then
            directory = trim(path_line(1:pos - 1))
            if (.not. is_safe_path(directory)) directory = ''
            return
        end if

        pos = index(path_line, '\fortfront.mod', back=.true.)
        if (pos > 0) then
            directory = trim(path_line(1:pos - 1))
            if (.not. is_safe_path(directory)) directory = ''
        end if
    end function directory_from_module_path

    function extract_module_candidate(path, marker) result(value)
        character(len=*), intent(in) :: path, marker
        character(len=:), allocatable :: value
        integer :: pos

        value = ''
        pos = index(path, marker, back=.true.)
        if (pos > 0) then
            value = trim(path(1:pos - 1))
        end if
    end function extract_module_candidate

    logical function module_directory_has_module(base, sep)
        character(len=*), intent(in) :: base
        character(len=1), intent(in) :: sep
        character(len=:), allocatable :: module_path
        logical :: exists

        module_directory_has_module = .false.
        if (len_trim(base) == 0) return

        module_path = trim(base) // sep // 'fortfront.mod'
        inquire (file=trim(module_path), exist=exists)
        if (exists) then
            module_directory_has_module = is_safe_path(base)
        end if
    end function module_directory_has_module

    function build_compile_command(output_file, module_dir, is_windows) result(command)
        character(len=*), intent(in) :: output_file
        character(len=*), intent(in) :: module_dir
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: command
        character(len=:), allocatable :: module_arg, output_arg

        command = ''

        output_arg = quote_for_shell(output_file, is_windows)
        if (len_trim(output_arg) == 0) return

        command = 'gfortran -c -fsyntax-only '

        if (len_trim(module_dir) > 0) then
            module_arg = quote_for_shell(module_dir, is_windows)
            if (len_trim(module_arg) > 0) then
                command = command // '-I ' // module_arg // ' '
            end if
        end if

        command = command // output_arg

        if (is_windows) then
            command = command // ' > nul 2>&1'
        else
            command = command // ' > /dev/null 2>&1'
        end if
    end function build_compile_command

    function quote_for_shell(path, is_windows) result(argument)
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: argument

        if (.not. is_safe_path(path)) then
            argument = ''
        else
            if (is_windows) then
                argument = '"' // trim(path) // '"'
            else
                argument = '"' // trim(path) // '"'
            end if
        end if
    end function quote_for_shell

    logical function is_safe_path(path)
        character(len=*), intent(in) :: path
        integer :: i
        character(len=*), parameter :: forbidden_chars = "'""&|;<>`$"

        is_safe_path = .false.
        if (len_trim(path) == 0) return

        do i = 1, len_trim(path)
            if (index(forbidden_chars, path(i:i)) > 0) return
        end do

        if (index(path, achar(10)) > 0) return
        if (index(path, achar(13)) > 0) return

        is_safe_path = .true.
    end function is_safe_path

end program test_all_examples
