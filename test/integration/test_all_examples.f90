program test_all_examples
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
    use executable_finder, only: find_fortfront_executable
    implicit none

    integer :: test_count, pass_count, fail_count, skip_count
    integer :: xfail_count, xpass_count
    logical :: is_windows
    character(len=256) :: examples_dir
    character(len=256), allocatable :: expected_failures(:)
    integer :: num_expected_failures
    character(len=:), allocatable :: fortfront_exe
    real(dp) :: success_rate

    test_count = 0
    pass_count = 0
    fail_count = 0
    skip_count = 0
    xfail_count = 0
    xpass_count = 0

    is_windows = check_if_windows()
    call verify_shell_helpers(is_windows)

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
        success_rate = real(pass_count + xfail_count, kind=dp) * 100.0_dp / &
                       real(test_count, kind=dp)
        write (*, '(A,F6.1,A)') "Success rate: ", success_rate, "%"
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

    subroutine verify_shell_helpers(is_windows)
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: quoted
        character(len=:), allocatable :: command
        integer :: trimmed_len

        quoted = quote_for_shell('path with spaces/example.lf', is_windows)
        if (len_trim(quoted) == 0) then
            print *, "ERROR: quote_for_shell rejected safe path"
            stop 1
        end if
        trimmed_len = len_trim(quoted)
        if (quoted(1:1) /= '"' .or. quoted(trimmed_len:trimmed_len) /= '"') then
            print *, "ERROR: quote_for_shell missing quotes"
            stop 1
        end if

        if (len_trim(quote_for_shell('bad&path', is_windows)) /= 0 .or. &
            len_trim(quote_for_shell('bad%path', is_windows)) /= 0) then
            print *, "ERROR: quote_for_shell accepted unsafe characters"
            stop 1
        end if

        command = build_compile_command('output file.f90', 'modules dir', is_windows)
        if (len_trim(command) == 0) then
            print *, "ERROR: build_compile_command returned empty command"
            stop 1
        end if
        if (is_windows) then
            if (index(command, '""modules dir""') == 0) then
                print *, "ERROR: module directory not quoted for cmd"
                stop 1
            end if
            if (index(command, '""output file.f90""') == 0) then
                print *, "ERROR: output path not quoted for cmd"
                stop 1
            end if
        else
            if (index(command, '"modules dir"') == 0) then
                print *, "ERROR: module directory not quoted"
                stop 1
            end if
            if (index(command, '"output file.f90"') == 0) then
                print *, "ERROR: output path not quoted"
                stop 1
            end if
        end if
        if (is_windows) then
            if (index(quote_for_shell('pipe path', is_windows, &
                                      escape_for_cmd=.true.), '""pipe path""') == 0) then
                print *, "ERROR: Windows cmd escaping missing"
                stop 1
            end if
        end if
    end subroutine verify_shell_helpers

    subroutine cleanup_file(file)
        character(len=*), intent(in) :: file
        logical :: exists
        integer :: unit_num, ios
        character(len=:), allocatable :: trimmed

        trimmed = trim(file)
        if (len_trim(trimmed) == 0) return

        inquire (file=trimmed, exist=exists)
        if (.not. exists) return

        open (newunit=unit_num, file=trimmed, status='old', action='readwrite', &
            & iostat=ios)
        if (ios /= 0) then
            open (newunit=unit_num, file=trimmed, status='old', action='read', iostat=ios)
        end if
        if (ios == 0) then
            close (unit_num, status='delete', iostat=ios)
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

    pure function extract_example_basename(filepath) result(name)
        character(len=*), intent(in) :: filepath
        character(len=256) :: name
        character(len=:), allocatable :: trimmed
        integer :: sep_pos

        name = ''
        if (len_trim(filepath) == 0) return

        trimmed = trim(filepath)
        sep_pos = find_last_separator(trimmed)

        if (sep_pos > 0 .and. sep_pos < len(trimmed)) then
            name = trim(trimmed(sep_pos + 1:))
        else
            name = trim(trimmed)
        end if
        name = adjustl(name)
    end function extract_example_basename

    subroutine test_single_example(filepath, fortfront_exe, test_count, pass_count, &
                                   fail_count, skip_count, xfail_count, xpass_count, &
                                   is_windows, expected_failures, num_expected_failures)
        character(len=*), intent(in) :: filepath, fortfront_exe
        integer, intent(inout) :: test_count, pass_count, fail_count, skip_count
        integer, intent(inout) :: xfail_count, xpass_count
        logical, intent(in) :: is_windows
        character(len=256), intent(in) :: expected_failures(:)
        integer, intent(in) :: num_expected_failures

        character(len=256) :: output_file, error_file
        character(len=256) :: basename_str
        logical :: has_error, has_unparsed, has_warning, file_exists, expect_fail
        character(len=:), allocatable :: module_dir

        basename_str = extract_example_basename(filepath)
        output_file = 'test_example_' // trim(basename_str) // '_output.f90'
        error_file = 'test_example_' // trim(basename_str) // '.err'
        write (*, '(A)', advance='no') "Testing " // trim(basename_str) // " ... "

        inquire (file=trim(filepath), exist=file_exists)
        if (.not. file_exists) then
            print *, "SKIP (file not found)"
            skip_count = skip_count + 1
            return
        end if

        module_dir = get_module_directory(fortfront_exe)

        call run_transform_and_scan(filepath, fortfront_exe, output_file, &
                                    error_file, is_windows, has_error, has_unparsed, &
                                    has_warning)

        if (.not. has_error .and. .not. has_unparsed) then
            if (.not. compile_generated_output(output_file, module_dir, &
                                               is_windows)) then
                has_error = .true.
            end if
        end if

        expect_fail = is_expected_failure(trim(basename_str), expected_failures, &
                                          num_expected_failures)
        test_count = test_count + 1

        call finalize_example_result(trim(basename_str), output_file, error_file, &
                                     has_error, has_unparsed, has_warning, expect_fail, &
                                     pass_count, fail_count, xfail_count, xpass_count)

        call cleanup_file(output_file)
        call cleanup_file(error_file)
    end subroutine test_single_example

    subroutine run_transform_and_scan(filepath, fortfront_exe, output_file, &
                                      error_file, is_windows, has_error, has_unparsed, &
                                      has_warning)
        character(len=*), intent(in) :: filepath, fortfront_exe
        character(len=*), intent(in) :: output_file, error_file
        logical, intent(in) :: is_windows
        logical, intent(out) :: has_error, has_unparsed, has_warning
        character(len=2048) :: command
        character(len=:), allocatable :: input_arg, exe_arg, output_arg, error_arg
        integer :: exit_code

        input_arg = quote_for_shell(filepath, is_windows, &
                                    escape_for_cmd=.true.)
        exe_arg = quote_for_shell(fortfront_exe, is_windows, &
                                  escape_for_cmd=.true.)
        output_arg = quote_for_shell(output_file, is_windows, &
                                     escape_for_cmd=.true.)
        error_arg = quote_for_shell(error_file, is_windows, &
                                    escape_for_cmd=.true.)

        if (len_trim(input_arg) == 0 .or. len_trim(exe_arg) == 0 .or. &
            len_trim(output_arg) == 0 .or. len_trim(error_arg) == 0) then
            has_error = .true.
            has_unparsed = .false.
            has_warning = .false.
            return
        end if

        if (is_windows) then
            command = 'cmd /C "type ' // trim(input_arg) // ' | ' // trim(exe_arg) // &
                      ' > ' // trim(output_arg) // ' 2> ' // trim(error_arg) // '"'
        else
            command = 'cat ' // trim(input_arg) // ' | ' // trim(exe_arg) // &
                      ' > ' // trim(output_arg) // ' 2> ' // trim(error_arg)
        end if

        call execute_command_line(trim(command), exitstat=exit_code)

        has_error = (exit_code /= 0)
        has_unparsed = .false.
        has_warning = .false.

        call scan_error_file(error_file, has_error, has_warning)
        call scan_output_file(output_file, has_error, has_unparsed)
    end subroutine run_transform_and_scan

    subroutine scan_error_file(error_file, has_error, has_warning)
        character(len=*), intent(in) :: error_file
        logical, intent(inout) :: has_error, has_warning
        integer :: unit_num, ios
        character(len=512) :: line
        logical :: exists

        inquire (file=trim(error_file), exist=exists)
        if (.not. exists) then
            has_error = .true.
            return
        end if

        open (newunit=unit_num, file=trim(error_file), status='old', &
              action='read', iostat=ios)
        if (ios /= 0) then
            has_error = .true.
            return
        end if

        do
            read (unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, 'ERROR') > 0) has_error = .true.
            if (index(line, 'WARNING') > 0) has_warning = .true.
        end do

        close (unit_num)
    end subroutine scan_error_file

    subroutine scan_output_file(output_file, has_error, has_unparsed)
        character(len=*), intent(in) :: output_file
        logical, intent(inout) :: has_error, has_unparsed
        integer :: unit_num, ios
        character(len=512) :: line
        logical :: exists

        inquire (file=trim(output_file), exist=exists)
        if (.not. exists) then
            has_error = .true.
            return
        end if

        open (newunit=unit_num, file=trim(output_file), status='old', &
              action='read', iostat=ios)
        if (ios /= 0) then
            has_error = .true.
            return
        end if

        do
            read (unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, '! COMPILATION FAILED') > 0) then
                has_error = .true.
                exit
            end if
            if (index(line, '! Unparsed:') > 0) has_unparsed = .true.
        end do

        close (unit_num)
    end subroutine scan_output_file

    logical function compile_generated_output(output_file, module_dir, is_windows)
        character(len=*), intent(in) :: output_file
        character(len=*), intent(in) :: module_dir
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: command
        integer :: exit_code

        command = build_compile_command(output_file, module_dir, is_windows)
        if (len_trim(command) == 0) then
            compile_generated_output = .false.
            return
        end if

        call execute_command_line(trim(command), exitstat=exit_code)
        compile_generated_output = (exit_code == 0)
    end function compile_generated_output

    subroutine finalize_example_result(name, output_file, error_file, has_error, &
                                       has_unparsed, has_warning, expect_fail, &
                                       pass_count, fail_count, xfail_count, xpass_count)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: output_file, error_file
        logical, intent(in) :: has_error, has_unparsed, has_warning, expect_fail
        integer, intent(inout) :: pass_count, fail_count, xfail_count, xpass_count

        if (has_error .or. has_unparsed) then
            if (expect_fail) then
                print *, "XFAIL (expected failure)"
                xfail_count = xfail_count + 1
            else
                call report_example_failure(name, output_file, error_file)
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
    end subroutine finalize_example_result

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
        character(len=:), allocatable :: current_dir
        character(len=:), allocatable :: parent_dir
        character(len=:), allocatable :: env_candidate
        character(len=1) :: sep
        character(len=1024) :: env_dir
        integer :: env_status

        module_dir = ''

        env_dir = ''
        call get_environment_variable('FORTFRONT_MODULE_DIR', env_dir, &
                                      status=env_status)
        if (env_status == 0) then
            env_candidate = trim(env_dir)
            if (len_trim(env_candidate) > 0) then
                if (index(env_candidate, '.mod', back=.true.) == &
                    len_trim(env_candidate) - 3) then
                    env_candidate = directory_from_path(env_candidate)
                end if
                if (len_trim(env_candidate) > 0) then
                    sep = path_separator_for(env_candidate)
                    if (module_directory_has_module(env_candidate, sep)) then
                        module_dir = trim(env_candidate)
                        return
                    end if
                end if
            end if
        end if

        candidate = find_module_dir_from_compile_commands(executable_path)
        if (len_trim(candidate) > 0) then
            module_dir = trim(candidate)
            return
        end if

        current_dir = directory_from_path(executable_path)
        do while (len_trim(current_dir) > 0)
            if (set_module_dir_if_exists(current_dir, module_dir)) return
            parent_dir = directory_from_path(current_dir)
            if (len_trim(parent_dir) == 0) exit
            if (trim(parent_dir) == trim(current_dir)) exit
            current_dir = parent_dir
        end do

        candidate = extract_module_candidate(executable_path, '/app/')
        if (len_trim(candidate) > 0) then
            if (set_module_dir_if_exists(candidate, module_dir)) return
        end if

        candidate = extract_module_candidate(executable_path, '\app\')
        if (len_trim(candidate) > 0) then
            if (set_module_dir_if_exists(candidate, module_dir)) return
        end if

        sep = path_separator_for('fortfront_modules')
        if (module_directory_has_module('fortfront_modules', sep)) then
            module_dir = 'fortfront_modules'
        end if
    end function get_module_directory

    pure function extract_module_candidate(path, marker) result(value)
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

    function find_module_dir_from_compile_commands(executable_path) result(module_dir)
        character(len=*), intent(in) :: executable_path
        character(len=:), allocatable :: module_dir
        integer :: unit_num, ios
        character(len=512) :: line
        logical :: awaiting_path
        character(len=:), allocatable :: candidate
        character(len=1) :: sep
        character(len=:), allocatable :: commands_path
        logical :: exists

        module_dir = ''
        awaiting_path = .false.

        commands_path = 'build/compile_commands.json'
        inquire (file=trim(commands_path), exist=exists)
        if (.not. exists) then
            commands_path = resolve_compile_commands_path(executable_path)
            if (len_trim(commands_path) == 0) return
        end if

        open (newunit=unit_num, file=trim(commands_path), status='old', &
              action='read', iostat=ios)
        if (ios /= 0) return

        do
            read (unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit

            if (awaiting_path) then
                candidate = extract_argument_path(line)
                if (len_trim(candidate) > 0) then
                    sep = path_separator_for(candidate)
                    if (module_directory_has_module(candidate, sep)) then
                        module_dir = trim(candidate)
                        exit
                    end if
                end if
                awaiting_path = .false.
            else if (index(line, '"-J"') > 0) then
                awaiting_path = .true.
            end if
        end do

        close (unit_num)
    end function find_module_dir_from_compile_commands

    function resolve_compile_commands_path(executable_path) result(path)
        character(len=*), intent(in) :: executable_path
        character(len=:), allocatable :: path
        character(len=:), allocatable :: current_dir
        character(len=:), allocatable :: candidate
        character(len=1) :: sep
        logical :: exists

        path = ''
        current_dir = directory_from_path(executable_path)

        do while (len_trim(current_dir) > 0)
            sep = path_separator_for(current_dir)
            candidate = join_path(current_dir, 'compile_commands.json', sep)
            inquire (file=trim(candidate), exist=exists)
            if (exists) then
                path = trim(candidate)
                return
            end if
            current_dir = directory_from_path(current_dir)
        end do

        inquire (file='compile_commands.json', exist=exists)
        if (exists) path = 'compile_commands.json'
    end function resolve_compile_commands_path

    pure function extract_argument_path(line) result(path)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: path
        integer :: first_quote, second_quote

        path = ''

        first_quote = index(line, '"')
        if (first_quote == 0) return

        second_quote = index(line(first_quote + 1:), '"')
        if (second_quote == 0) return

        second_quote = second_quote + first_quote
        if (second_quote - first_quote <= 1) return

        path = trim(line(first_quote + 1:second_quote - 1))
    end function extract_argument_path

    pure function join_path(base, component, sep) result(path)
        character(len=*), intent(in) :: base
        character(len=*), intent(in) :: component
        character(len=1), intent(in) :: sep
        character(len=:), allocatable :: path
        character(len=:), allocatable :: trimmed_base
        integer :: last_char

        path = ''

        if (len_trim(component) == 0) then
            path = trim(base)
            return
        end if

        trimmed_base = trim(base)
        if (len(trimmed_base) == 0) then
            path = trim(component)
            return
        end if

        last_char = len(trimmed_base)
        if (trimmed_base(last_char:last_char) == sep) then
            path = trimmed_base // trim(component)
        else
            path = trimmed_base // sep // trim(component)
        end if
    end function join_path

    pure function directory_from_path(path) result(directory)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: directory
        character(len=:), allocatable :: trimmed_path
        integer :: sep_pos

        directory = ''

        trimmed_path = trim(path)
        if (len(trimmed_path) == 0) return

        sep_pos = find_last_separator(trimmed_path)
        if (sep_pos <= 0) then
            directory = ''
        else if (sep_pos == 1) then
            directory = trimmed_path(1:1)
        else
            directory = trim(trimmed_path(1:sep_pos - 1))
        end if
    end function directory_from_path

    pure integer function find_last_separator(path) result(position)
        character(len=*), intent(in) :: path
        integer :: i

        position = 0
        do i = len(path), 1, -1
            if (path(i:i) == '/' .or. path(i:i) == '\') then
                position = i
                return
            end if
        end do
    end function find_last_separator

    function path_separator_for(path) result(sep)
        character(len=*), intent(in) :: path
        character(len=1) :: sep
        integer :: pos
        logical :: is_win

        sep = '/'

        if (len_trim(path) == 0) then
            is_win = check_if_windows()
            if (is_win) sep = '\'
            return
        end if

        pos = find_last_separator(path)
        if (pos > 0) then
            sep = path(pos:pos)
        else
            is_win = check_if_windows()
            if (is_win) sep = '\'
        end if
    end function path_separator_for

    logical function set_module_dir_if_exists(base, module_dir)
        character(len=*), intent(in) :: base
        character(len=:), allocatable, intent(inout) :: module_dir
        character(len=1) :: sep
        character(len=:), allocatable :: candidate

        set_module_dir_if_exists = .false.
        if (len_trim(base) == 0) return

        sep = path_separator_for(base)
        if (module_directory_has_module(base, sep)) then
            module_dir = trim(base)
            set_module_dir_if_exists = .true.
            return
        end if

        candidate = join_path(base, 'build', sep)
        if (len_trim(candidate) == 0) return
        if (module_directory_has_module(candidate, sep)) then
            module_dir = trim(candidate)
            set_module_dir_if_exists = .true.
        end if
    end function set_module_dir_if_exists

    pure function build_compile_command(output_file, module_dir, is_windows) result(command)
        character(len=*), intent(in) :: output_file
        character(len=*), intent(in) :: module_dir
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: command
        character(len=:), allocatable :: module_arg, output_arg

        command = ''

        output_arg = quote_for_shell(output_file, is_windows, &
                                     escape_for_cmd=is_windows)
        if (len_trim(output_arg) == 0) return

        command = 'gfortran -c -fsyntax-only '

        if (len_trim(module_dir) > 0) then
            module_arg = quote_for_shell(module_dir, is_windows, &
                                         escape_for_cmd=is_windows)
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

    pure function quote_for_shell(path, is_windows, escape_for_cmd) result(argument)
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_windows
        logical, intent(in), optional :: escape_for_cmd
        character(len=:), allocatable :: argument
        logical :: needs_cmd_escape

        needs_cmd_escape = .false.
        if (present(escape_for_cmd)) needs_cmd_escape = escape_for_cmd

        if (.not. is_safe_path(path)) then
            argument = ''
        else if (is_windows .and. needs_cmd_escape) then
            argument = '""' // trim(path) // '""'
        else
            argument = '"' // trim(path) // '"'
        end if
    end function quote_for_shell

    pure logical function is_safe_path(path)
        character(len=*), intent(in) :: path
        integer :: i
        integer :: code
        character(len=*), parameter :: forbidden_chars = "'""&|;<>`$%^"

        is_safe_path = .false.
        if (len_trim(path) == 0) return

        do i = 1, len_trim(path)
            code = iachar(path(i:i))
            if (code < 32 .or. code == 127) return
            if (index(forbidden_chars, path(i:i)) > 0) return
        end do

        if (index(path, achar(10)) > 0) return
        if (index(path, achar(13)) > 0) return

        is_safe_path = .true.
    end function is_safe_path

end program test_all_examples
