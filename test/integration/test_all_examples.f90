program test_all_examples
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count, fail_count, skip_count
    integer :: xfail_count, xpass_count
    logical :: is_windows
    character(len=256) :: examples_dir
    character(len=256), allocatable :: expected_failures(:)
    integer :: num_expected_failures
    integer :: ios

    test_count = 0
    pass_count = 0
    fail_count = 0
    skip_count = 0
    xfail_count = 0
    xpass_count = 0

    is_windows = check_if_windows()

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
    call test_examples_by_extension(examples_dir, '.lf', &
                                     test_count, pass_count, fail_count, skip_count, &
                                     xfail_count, xpass_count, is_windows, &
                                     expected_failures, num_expected_failures)

    ! Test .f90 (standard fortran) examples
    call test_examples_by_extension(examples_dir, '.f90', &
                                     test_count, pass_count, fail_count, skip_count, &
                                     xfail_count, xpass_count, is_windows, &
                                     expected_failures, num_expected_failures)

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0)') "Total examples tested: ", test_count
    write(*, '(A,I0)') "Passed: ", pass_count
    write(*, '(A,I0)') "Failed: ", fail_count
    write(*, '(A,I0)') "XFail (expected): ", xfail_count
    write(*, '(A,I0)') "XPass (unexpected): ", xpass_count
    write(*, '(A,I0)') "Skipped: ", skip_count

    if (test_count > 0) then
        write(*, '(A,F5.1,A)') "Success rate: ", &
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
        if (check_if_windows()) then
            call execute_command_line('cmd /C if exist ' // trim(file) // &
                                      ' del /F /Q ' // trim(file), exitstat=ec)
        else
            call execute_command_line('rm -f ' // trim(file), exitstat=ec)
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

        inquire(file=trim(filename), exist=file_exists)
        if (.not. file_exists) then
            allocate(failures(0))
            return
        end if

        ! First pass: count non-empty, non-comment lines
        open(newunit=unit_num, file=trim(filename), status='old', &
             action='read', iostat=ios)
        if (ios /= 0) then
            allocate(failures(0))
            return
        end if

        count = 0
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            trimmed_line = adjustl(line)
            if (len_trim(trimmed_line) > 0 .and. trimmed_line(1:1) /= '#') then
                count = count + 1
            end if
        end do
        close(unit_num)

        ! Allocate array
        allocate(failures(count))
        num_failures = count

        ! Second pass: read filenames
        open(newunit=unit_num, file=trim(filename), status='old', &
             action='read', iostat=ios)
        i = 0
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            trimmed_line = adjustl(line)
            if (len_trim(trimmed_line) > 0 .and. trimmed_line(1:1) /= '#') then
                i = i + 1
                ! Extract filename before any # comment
                if (index(trimmed_line, '#') > 0) then
                    failures(i) = adjustl(trimmed_line(1:index(trimmed_line, '#')-1))
                else
                    failures(i) = trim(trimmed_line)
                end if
            end if
        end do
        close(unit_num)
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

    subroutine test_examples_by_extension(examples_dir, extension, &
                                           test_count, pass_count, fail_count, skip_count, &
                                           xfail_count, xpass_count, is_windows, &
                                           expected_failures, num_expected_failures)
        character(len=*), intent(in) :: examples_dir, extension
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
                           trim(extension) // ' > ' // trim(list_file) // ' 2>/dev/null || true'
        end if

        call execute_command_line(trim(list_command), exitstat=ios)

        ! Read and test each example file
        open(newunit=unit_num, file=trim(list_file), status='old', &
             action='read', iostat=ios)

        if (ios /= 0) then
            ! No files found with this extension, that's ok
            call cleanup_file(list_file)
            return
        end if

        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit

            if (len_trim(line) == 0) cycle

            ! Build full path
            if (is_windows) then
                call test_single_example(trim(examples_dir) // '\' // trim(line), &
                                          test_count, pass_count, fail_count, skip_count, &
                                          xfail_count, xpass_count, is_windows, &
                                          expected_failures, num_expected_failures)
            else
                ! On Unix, ls gives full path already
                call test_single_example(trim(line), &
                                          test_count, pass_count, fail_count, skip_count, &
                                          xfail_count, xpass_count, is_windows, &
                                          expected_failures, num_expected_failures)
            end if
        end do

        close(unit_num)
        call cleanup_file(list_file)

    end subroutine test_examples_by_extension

    subroutine test_single_example(filepath, test_count, pass_count, &
                                     fail_count, skip_count, xfail_count, xpass_count, &
                                     is_windows, expected_failures, num_expected_failures)
        character(len=*), intent(in) :: filepath
        integer, intent(inout) :: test_count, pass_count, fail_count, skip_count
        integer, intent(inout) :: xfail_count, xpass_count
        logical, intent(in) :: is_windows
        character(len=256), intent(in) :: expected_failures(:)
        integer, intent(in) :: num_expected_failures

        character(len=500) :: command
        character(len=256) :: output_file, error_file, basename_str
        integer :: exit_code, i, unit_out
        logical :: has_error, has_unparsed, file_exists, has_warning, expect_fail
        character(len=512) :: line

        ! Extract basename for display and output files
        i = max(index(filepath, '/', back=.true.), index(filepath, '\', back=.true.))
        if (i > 0) then
            basename_str = filepath(i+1:)
        else
            basename_str = filepath
        end if

        output_file = 'test_example_' // trim(basename_str) // '_output.f90'
        error_file = 'test_example_' // trim(basename_str) // '.err'

        test_count = test_count + 1

        write(*, '(A)', advance='no') "Testing " // trim(basename_str) // " ... "

        ! Check if example file exists
        inquire(file=trim(filepath), exist=file_exists)
        if (.not. file_exists) then
            print *, "SKIP (file not found)"
            skip_count = skip_count + 1
            return
        end if

        ! Run fortfront on the example using fpm run
        ! Note: fpm output goes to stderr, actual fortran code to stdout
        if (is_windows) then
            command = 'cmd /C "type ' // trim(filepath) // ' | ' // &
                      'fpm run fortfront 2>nul > ' // &
                      trim(output_file) // '"'
        else
            command = 'sh -c "cat ' // trim(filepath) // ' | fpm run fortfront 2>' // &
                      trim(error_file) // ' > ' // trim(output_file) // '"'
        end if

        call execute_command_line(trim(command), exitstat=exit_code)

        ! Check for errors in output and stderr
        has_error = .false.
        has_unparsed = .false.
        has_warning = .false.

        ! Check stderr file for error markers (parser errors go to stderr)
        open(newunit=unit_out, file=trim(error_file), status='old', &
             action='read', iostat=exit_code)
        if (exit_code == 0) then
            do
                read(unit_out, '(A)', iostat=exit_code) line
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
            close(unit_out)
        end if

        ! Check output file for code generation issues
        open(newunit=unit_out, file=trim(output_file), status='old', &
             action='read', iostat=exit_code)
        if (exit_code == 0) then
            do
                read(unit_out, '(A)', iostat=exit_code) line
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
            close(unit_out)
        else
            has_error = .true.
        end if

        ! Try to compile the output with gfortran to catch silent bugs
        if (.not. has_error .and. .not. has_unparsed) then
            if (is_windows) then
                command = 'gfortran -c -fsyntax-only ' // trim(output_file) // &
                          ' > nul 2>&1'
            else
                command = 'gfortran -c -fsyntax-only ' // trim(output_file) // &
                          ' > /dev/null 2>&1'
            end if
            call execute_command_line(trim(command), exitstat=exit_code)
            if (exit_code /= 0) then
                has_error = .true.
            end if
        end if

        ! Clean up temp files
        call cleanup_file(output_file)
        call cleanup_file(error_file)

        ! Check if this test is expected to fail
        expect_fail = is_expected_failure(basename_str, expected_failures, &
                                           num_expected_failures)

        ! Report result
        if (has_error .or. has_unparsed) then
            if (expect_fail) then
                print *, "XFAIL (expected failure)"
                xfail_count = xfail_count + 1
            else
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

    end subroutine test_single_example

end program test_all_examples
