program test_all_examples
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
    use test_example_lists, only: assert_skip_path_handling, load_example_list
    use test_example_lists, only: load_skip_examples, is_analysis_only_example
    use test_example_lists, only: is_expected_diagnostic, is_expected_failure
    use test_example_lists, only: is_skipped_example
    use test_filesystem_helpers, only: check_if_windows, cleanup_file, &
                                       cleanup_temp_directory
    use test_filesystem_helpers, only: create_temp_directory, &
                                       extract_example_basename
    use test_filesystem_helpers, only: extract_relative_example_path
    use test_filesystem_helpers, only: find_fortfront_executable
    use test_filesystem_helpers, only: path_separator_for
    use test_module_discovery, only: get_module_directory
    use test_shell_commands, only: build_compile_command, quote_for_shell, &
                                   verify_shell_helpers
    implicit none

    integer :: test_count, pass_count, fail_count, skip_count
    integer :: xfail_count, xpass_count
    logical :: is_windows
    character(len=256) :: examples_dir
    character(len=256), allocatable :: expected_failures(:)
    character(len=256), allocatable :: skip_examples(:)
    character(len=256), allocatable :: analysis_only_examples(:)
    character(len=256), allocatable :: diagnostic_examples(:)
    integer :: num_expected_failures
    integer :: num_skip_examples
    integer :: num_analysis_only_examples
    integer :: num_diagnostic_examples
    character(len=:), allocatable :: fortfront_exe
    character(len=:), allocatable :: temp_dir
    character(len=:), allocatable :: example_timeout_launcher
    logical :: timeout_requires_command_string = .false.
    logical :: timeout_string_needs_quotes = .false.
    real(dp) :: success_rate

    test_count = 0
    pass_count = 0
    fail_count = 0
    skip_count = 0
    xfail_count = 0
    xpass_count = 0

    is_windows = check_if_windows()
    call verify_shell_helpers(is_windows)
    call initialize_example_timeout_launcher(is_windows)

    ! Create temp directory for test outputs
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) then
        print *, "ERROR: Could not create temporary directory"
        stop 1
    end if
    print *, "Using temp directory: ", trim(temp_dir)

    ! Find fortfront executable (avoid fpm run overhead)
    fortfront_exe = find_fortfront_executable(is_windows)
    if (len(fortfront_exe) == 0) then
        print *, "ERROR: Could not locate fortfront executable"
        print *, "Please run 'fpm build' before running tests"
        stop 1
    end if

    ! fpm test runs from project root, so examples/ is directly accessible
    examples_dir = 'examples'

    call assert_skip_path_handling()

    ! Load expected failures list
    call load_example_list('examples/expected_failures.txt', &
                           expected_failures, num_expected_failures)
    call load_skip_examples('examples/skip_all_examples.txt', skip_examples, &
                            num_skip_examples)
    call load_example_list('examples/analysis_only_examples.txt', &
                           analysis_only_examples, num_analysis_only_examples)
    call load_example_list('examples/expected_diagnostics.txt', &
                           diagnostic_examples, num_diagnostic_examples)

    print *, "=== Fortfront Examples Integration Test ==="
    print *, ""
    print *, "Testing all example files (.lf and .f90) in examples/ directory"
    print *, "This validates that documented examples work correctly"
    print *, ""

    ! Test .lf (lazy fortran) examples
    call test_examples_by_extension(examples_dir, '.lf', fortfront_exe, temp_dir, &
                                    test_count, pass_count, fail_count, skip_count, &
                                    xfail_count, xpass_count, is_windows, &
                                    expected_failures, num_expected_failures, &
                                    skip_examples, num_skip_examples, &
                                    analysis_only_examples, &
                                    num_analysis_only_examples, &
                                    diagnostic_examples, num_diagnostic_examples)

    ! Test .f90 (standard fortran) examples
    call test_examples_by_extension(examples_dir, '.f90', fortfront_exe, temp_dir, &
                                    test_count, pass_count, fail_count, skip_count, &
                                    xfail_count, xpass_count, is_windows, &
                                    expected_failures, num_expected_failures, &
                                    skip_examples, num_skip_examples, &
                                    analysis_only_examples, &
                                    num_analysis_only_examples, &
                                    diagnostic_examples, num_diagnostic_examples)

    call cleanup_temp_directory(temp_dir, is_windows)

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

    subroutine test_examples_by_extension(examples_dir, extension, fortfront_exe, &
                                          temp_dir, test_count, pass_count, &
                                          fail_count, &
                                          skip_count, xfail_count, xpass_count, &
                                          is_windows, expected_failures, &
                                          num_expected_failures, skip_examples, &
                                          num_skip_examples, analysis_only_examples, &
                                          num_analysis_only_examples, &
                                          diagnostic_examples, num_diagnostic_examples)
        character(len=*), intent(in) :: examples_dir, extension, fortfront_exe
        character(len=*), intent(in) :: temp_dir
        integer, intent(inout) :: test_count, pass_count, fail_count, skip_count
        integer, intent(inout) :: xfail_count, xpass_count
        logical, intent(in) :: is_windows
        character(len=256), intent(in) :: expected_failures(:)
        integer, intent(in) :: num_expected_failures
        character(len=256), intent(in) :: skip_examples(:)
        integer, intent(in) :: num_skip_examples
        character(len=256), intent(in) :: analysis_only_examples(:)
        integer, intent(in) :: num_analysis_only_examples
        character(len=256), intent(in) :: diagnostic_examples(:)
        integer, intent(in) :: num_diagnostic_examples

        character(len=500) :: list_command, list_file
        integer :: unit_num, ios
        character(len=256) :: line

        ! Create unique temp file name for this extension
        list_file = 'examples_list' // trim(extension) // '.txt'

        ! List files with this extension (recursively search subdirectories)
        if (is_windows) then
            list_command = 'cmd /C "dir /B /S ' // trim(examples_dir) // '\*' // &
                           trim(extension) // ' > ' // trim(list_file) // ' 2>nul"'
        else
            list_command = 'find ' // trim(examples_dir) // ' -name "*' // &
                           trim(extension) // '" -type f > ' // trim(list_file) // &
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

            ! On both Windows (dir /S) and Unix (find), we get full paths
            call test_single_example(trim(line), fortfront_exe, temp_dir, &
                                     test_count, pass_count, fail_count, &
                                     skip_count, &
                                     xfail_count, xpass_count, is_windows, &
                                     expected_failures, num_expected_failures, &
                                     skip_examples, num_skip_examples, &
                                     analysis_only_examples, &
                                     num_analysis_only_examples, &
                                     diagnostic_examples, num_diagnostic_examples)
        end do

        close (unit_num)
        call cleanup_file(list_file)

    end subroutine test_examples_by_extension

    subroutine test_single_example(filepath, fortfront_exe, temp_dir, test_count, &
                                   pass_count, fail_count, skip_count, xfail_count, &
                                   xpass_count, is_windows, expected_failures, &
                                   num_expected_failures, skip_examples, &
                                   num_skip_examples, analysis_only_examples, &
                                   num_analysis_only_examples, diagnostic_examples, &
                                   num_diagnostic_examples)
        character(len=*), intent(in) :: filepath, fortfront_exe, temp_dir
        integer, intent(inout) :: test_count, pass_count, fail_count, skip_count
        integer, intent(inout) :: xfail_count, xpass_count
        logical, intent(in) :: is_windows
        character(len=256), intent(in) :: expected_failures(:)
        integer, intent(in) :: num_expected_failures
        character(len=256), intent(in) :: skip_examples(:)
        integer, intent(in) :: num_skip_examples
        character(len=256), intent(in) :: analysis_only_examples(:)
        integer, intent(in) :: num_analysis_only_examples
        character(len=256), intent(in) :: diagnostic_examples(:)
        integer, intent(in) :: num_diagnostic_examples

        character(len=:), allocatable :: output_file, error_file
        character(len=256) :: basename_str
        character(len=256) :: relative_path
        character(len=1) :: sep
        logical :: has_error, has_unparsed, has_warning, file_exists, expect_fail
        logical :: is_f90_roundtrip
        logical :: analysis_only, expect_diagnostic
        character(len=:), allocatable :: module_dir

        basename_str = extract_example_basename(filepath)
        sep = path_separator_for(temp_dir)
        output_file = trim(temp_dir) // sep // 'test_example_' // &
                      trim(basename_str) // '_output.f90'
        error_file = trim(temp_dir) // sep // 'test_example_' // &
                     trim(basename_str) // '.err'
        is_f90_roundtrip = (index(filepath, '.f90') > 0)
        write (*, '(A)', advance='no') "Testing " // trim(basename_str) // " ... "

        inquire (file=trim(filepath), exist=file_exists)
        if (.not. file_exists) then
            print *, "SKIP (file not found)"
            skip_count = skip_count + 1
            return
        end if

        relative_path = extract_relative_example_path(filepath)
        if (is_skipped_example(relative_path, trim(basename_str), skip_examples, &
                               num_skip_examples)) then
            print *, "SKIP (covered by targeted regression)"
            skip_count = skip_count + 1
            return
        end if

        analysis_only = is_analysis_only_example(relative_path, &
                                                 trim(basename_str), &
                                                 analysis_only_examples, &
                                                 num_analysis_only_examples)
        expect_diagnostic = is_expected_diagnostic(relative_path, &
                                                   trim(basename_str), &
                                                   diagnostic_examples, &
                                                   num_diagnostic_examples)

        module_dir = get_module_directory(fortfront_exe)

        call run_transform_and_scan(filepath, fortfront_exe, output_file, &
                                    error_file, is_windows, is_f90_roundtrip, &
                                    has_error, has_unparsed, has_warning)

        if (.not. has_error .and. .not. has_unparsed .and. &
            .not. analysis_only) then
            if (.not. compile_generated_output(output_file, module_dir, &
                                               temp_dir, is_windows)) then
                has_error = .true.
            end if
        end if

        expect_fail = is_expected_failure(trim(basename_str), expected_failures, &
                                          num_expected_failures)
        test_count = test_count + 1

        call finalize_example_result(trim(basename_str), output_file, error_file, &
                                     has_error, has_unparsed, has_warning, &
                                     expect_fail, expect_diagnostic, analysis_only, &
                                     pass_count, fail_count, xfail_count, &
                                     xpass_count, is_f90_roundtrip)

        if (.not. is_f90_roundtrip) then
            call cleanup_file(output_file)
            call cleanup_file(error_file)
        end if
    end subroutine test_single_example

    subroutine run_transform_and_scan(filepath, fortfront_exe, output_file, &
                                      error_file, is_windows, is_f90_roundtrip, &
                                      has_error, has_unparsed, &
                                      has_warning)
        character(len=*), intent(in) :: filepath, fortfront_exe
        character(len=*), intent(in) :: output_file, error_file
        logical, intent(in) :: is_windows, is_f90_roundtrip
        logical, intent(out) :: has_error, has_unparsed, has_warning
        character(len=:), allocatable :: input_arg, exe_arg, output_arg, error_arg
        character(len=:), allocatable :: raw_command, timed_command
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

        raw_command = trim(exe_arg) // ' ' // trim(input_arg) // ' > ' // &
                      trim(output_arg) // ' 2> ' // trim(error_arg)

        timed_command = build_timed_command(raw_command)

        call execute_command_line(trim(timed_command), exitstat=exit_code)

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

    logical function compile_generated_output(output_file, module_dir, temp_dir, &
                                              is_windows)
        character(len=*), intent(in) :: output_file
        character(len=*), intent(in) :: module_dir, temp_dir
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: command
        integer :: exit_code

        command = build_compile_command(output_file, module_dir, temp_dir, is_windows)
        if (len_trim(command) == 0) then
            compile_generated_output = .false.
            return
        end if

        call execute_command_line(trim(command), exitstat=exit_code)
        if (exit_code /= 0) then
            print *, 'DEBUG compile failed:', trim(command)
        end if
        compile_generated_output = (exit_code == 0)
    end function compile_generated_output

    subroutine finalize_example_result(name, output_file, error_file, has_error, &
                                       has_unparsed, has_warning, expect_fail, &
                                       expect_diagnostic, is_analysis_only, &
                                       pass_count, fail_count, xfail_count, &
                                       xpass_count, &
                                       is_roundtrip)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: output_file, error_file
        logical, intent(in) :: has_error, has_unparsed, has_warning, expect_fail
        logical, intent(in) :: expect_diagnostic, is_analysis_only
        logical, intent(in) :: is_roundtrip
        integer, intent(inout) :: pass_count, fail_count, xfail_count, xpass_count

        if (expect_diagnostic) then
            if (has_error .or. has_unparsed) then
                print *, "PASS (expected diagnostic)"
                pass_count = pass_count + 1
            else
                print *, "FAIL (expected diagnostic but transformation succeeded)"
                fail_count = fail_count + 1
            end if
            return
        end if

        if (has_error .or. has_unparsed) then
            if (expect_fail) then
                print *, "XFAIL (expected failure)"
                xfail_count = xfail_count + 1
            else
                if (.not. is_roundtrip) then
                    call report_example_failure(name, output_file, error_file)
                end if
                if (has_error) then
                    if (is_roundtrip) then
                        print *, "FAIL (compilation failed)"
                    else
                        print *, "FAIL (parser error or compilation failed)"
                    end if
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
                if (is_analysis_only) then
                    print *, "PASS (analysis-only)"
                else
                    print *, "PASS"
                end if
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

    logical function compile_f90_example(filepath, module_dir, temp_dir, is_windows)
        character(len=*), intent(in) :: filepath
        character(len=*), intent(in) :: module_dir, temp_dir
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: command
        integer :: exit_code

        command = build_compile_command(filepath, module_dir, temp_dir, is_windows)
        if (len_trim(command) == 0) then
            compile_f90_example = .false.
            return
        end if

        call execute_command_line(trim(command), exitstat=exit_code)
        compile_f90_example = (exit_code == 0)
    end function compile_f90_example

    subroutine initialize_example_timeout_launcher(is_windows)
        logical, intent(in) :: is_windows
        logical :: exists
        integer :: ec
        character(len=:), allocatable :: launcher

        if (is_windows) then
            inquire (file='scripts\with_timeout.ps1', exist=exists)
            if (.not. exists) then
                print *, 'ERROR: scripts\with_timeout.ps1 is required for example timeouts on Windows'
                stop 1
            end if
            launcher = 'powershell -NoProfile -ExecutionPolicy Bypass -File scripts\with_timeout.ps1'
            example_timeout_launcher = trim(launcher)
            timeout_requires_command_string = .true.
            timeout_string_needs_quotes = .false.
        else
            inquire (file='scripts/with_timeout.sh', exist=exists)
            if (exists) then
                launcher = quote_for_shell('scripts/with_timeout.sh', .false.)
                example_timeout_launcher = trim(launcher)
                timeout_requires_command_string = .false.
                return
            end if
            call execute_command_line('command -v timeout >/dev/null 2>&1', exitstat=ec)
            if (ec == 0) then
                example_timeout_launcher = 'timeout'
                timeout_requires_command_string = .false.
                return
            end if
            print *, 'ERROR: Unable to enforce example timeouts (provide scripts/with_timeout.sh or install timeout command)'
            stop 1
        end if
    end subroutine initialize_example_timeout_launcher

    function build_timed_command(raw_command) result(full_command)
        character(len=*), intent(in) :: raw_command
        character(len=:), allocatable :: full_command
        character(len=:), allocatable :: quoted_cmd

        if (.not. allocated(example_timeout_launcher)) then
            print *, 'ERROR: Example timeout launcher not initialized'
            stop 1
        end if

        if (timeout_requires_command_string) then
            if (timeout_string_needs_quotes) then
                quoted_cmd = quote_for_shell(trim(raw_command), .true., &
                                             escape_for_cmd=.true.)
                full_command = trim(example_timeout_launcher) // ' 1 ' // &
                               trim(quoted_cmd)
            else
                full_command = trim(example_timeout_launcher) // ' 1 ' // &
                               trim(raw_command)
            end if
        else
            full_command = trim(example_timeout_launcher) // ' 1 ' // &
                           trim(raw_command)
        end if
    end function build_timed_command

end program test_all_examples
