program test_all_examples
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
    implicit none

    integer :: test_count, pass_count, fail_count, skip_count
    integer :: xfail_count, xpass_count
    logical :: is_windows
    character(len=256) :: examples_dir
    character(len=256), allocatable :: expected_failures(:)
    character(len=256), allocatable :: skip_examples(:)
    integer :: num_expected_failures
    integer :: num_skip_examples
    character(len=:), allocatable :: fortfront_exe
    character(len=:), allocatable :: temp_dir
    real(dp) :: success_rate

    test_count = 0
    pass_count = 0
    fail_count = 0
    skip_count = 0
    xfail_count = 0
    xpass_count = 0

    is_windows = check_if_windows()
    call verify_shell_helpers(is_windows)

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

    ! Load expected failures list
    call load_expected_failures('examples/expected_failures.txt', &
                                expected_failures, num_expected_failures)
    call load_skip_examples('examples/skip_all_examples.txt', skip_examples, &
                            num_skip_examples)

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
                                    skip_examples, num_skip_examples)

    ! Test .f90 (standard fortran) examples
    call test_examples_by_extension(examples_dir, '.f90', fortfront_exe, temp_dir, &
                                    test_count, pass_count, fail_count, skip_count, &
                                    xfail_count, xpass_count, is_windows, &
                                    expected_failures, num_expected_failures, &
                                    skip_examples, num_skip_examples)

    ! Clean up temp directory
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

    function find_fortfront_executable(is_windows) result(executable_path)
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: executable_path
        logical :: file_exists
        character(len=500) :: candidate_path
        integer :: exit_code, unit_num, ios, r, i
        character(len=256) :: search_output
        character(len=64), allocatable :: roots(:)
        character(len=50), dimension(20) :: build_patterns

        executable_path = ''
        if (is_windows) then
            allocate (roots(5))
            roots = [character(len=16) :: '.', '..', '..\\..', '..\\..\\..', &
                     '..\\..\\..\\..']
            do r = 1, size(roots)
                call execute_command_line( &
                    'cmd /C where /R '//trim(roots(r))// &
                    ' fortfront.exe > fortfront_search_win.txt', &
                    exitstat=exit_code)
                if (exit_code == 0) then
                    open (newunit=unit_num, file='fortfront_search_win.txt', &
                          status='old', action='read', iostat=ios)
                    if (ios == 0) then
                        do
                            read (unit_num, '(A)', iostat=ios) search_output
                            if (ios /= 0) exit
                            if (len_trim(search_output) > 0) then
                                inquire (file=trim(search_output), exist=file_exists)
                                if (file_exists) then
                                    executable_path = trim(search_output)
                                    if (index(adjustl(search_output), &
                                              'app\\fortfront.exe') > 0) exit
                                end if
                            end if
                        end do
                        if (len(executable_path) == 0) then
                            rewind (unit_num)
                            read (unit_num, '(A)', iostat=ios) search_output
                            if (ios == 0 .and. len_trim(search_output) > 0) then
                                inquire (file=trim(search_output), exist=file_exists)
                                if (file_exists) executable_path = trim(search_output)
                            end if
                        end if
                        close (unit_num)
                    end if
                    call execute_command_line( &
                        'cmd /C del /F /Q fortfront_search_win.txt', &
                        exitstat=exit_code)
                end if
                if (len(executable_path) > 0) exit
            end do
            if (len(executable_path) > 0) return
            candidate_path = 'app\\fortfront.exe'
            inquire (file=candidate_path, exist=file_exists)
            if (file_exists) executable_path = trim(candidate_path)
            return
        end if

        call execute_command_line( &
            'find build -name "fortfront" -type f | head -1 > fortfront_search.txt', &
            exitstat=exit_code)
        if (exit_code == 0) then
            open (newunit=unit_num, file='fortfront_search.txt', status='old', &
                  action='read', iostat=ios)
            if (ios == 0) then
                read (unit_num, '(A)', iostat=ios) search_output
                close (unit_num)
                call execute_command_line('rm -f fortfront_search.txt', &
                                          exitstat=exit_code)
                if (ios == 0 .and. len_trim(search_output) > 0) then
                    inquire (file=trim(search_output), exist=file_exists)
                    if (file_exists) then
                        executable_path = trim(search_output)
                        return
                    end if
                end if
            end if
        end if

        build_patterns = [ &
                         'build/gfortran_266FF454AB2555FE/app/fortfront   ', &
                         'build/gfortran_9ABCD662468F5A74/app/fortfront   ', &
                         'build/gfortran_C79DEB301B8081FC/app/fortfront   ', &
                         'build/gfortran_C523F0F8A99FF060/app/fortfront   ', &
                         'build/gfortran_1F2DC83CBD1DC595/app/fortfront   ', &
                         'build/gfortran_35CFD5CFC35942D6/app/fortfront   ', &
                         'build/gfortran_4AE9E4ED7A89B913/app/fortfront   ', &
                         'build/gfortran_66DBF6172AF51040/app/fortfront   ', &
                         'build/gfortran_A56298966DD7666C/app/fortfront   ', &
                         'build/gfortran_E3D58E6D75301430/app/fortfront   ', &
                         'build/gfortran_9CBC8EEC13D00A4A/app/fortfront   ', &
                         './build/gfortran_266FF454AB2555FE/app/fortfront ', &
                         './build/gfortran_9ABCD662468F5A74/app/fortfront ', &
                         './build/gfortran_C79DEB301B8081FC/app/fortfront ', &
                         './build/gfortran_C523F0F8A99FF060/app/fortfront ', &
                         'fortfront                                       ', &
                         './fortfront                                     ', &
                         'app/fortfront                                   ', &
                         './app/fortfront                                 ', &
                         '../fortfront                                    ']

        do i = 1, size(build_patterns)
            candidate_path = trim(build_patterns(i))
            inquire (file=candidate_path, exist=file_exists)
            if (file_exists) then
                executable_path = trim(candidate_path)
                return
            end if
        end do
    end function find_fortfront_executable

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

        command = build_compile_command('output file.f90', 'modules dir', &
                                        'temp dir', is_windows)
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
                                      escape_for_cmd=.true.), &
                      '""pipe path""') == 0) then
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
            open (newunit=unit_num, file=trimmed, status='old', action='read', &
                  iostat=ios)
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

    subroutine load_skip_examples(filename, skip_list, num_skip)
        character(len=*), intent(in) :: filename
        character(len=256), allocatable, intent(out) :: skip_list(:)
        integer, intent(out) :: num_skip
        integer :: unit_num, ios, count, i
        character(len=256) :: line, trimmed_line
        logical :: file_exists

        num_skip = 0

        inquire (file=trim(filename), exist=file_exists)
        if (.not. file_exists) then
            allocate (skip_list(0))
            return
        end if

        open (newunit=unit_num, file=trim(filename), status='old', &
              action='read', iostat=ios)
        if (ios /= 0) then
            allocate (skip_list(0))
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

        allocate (skip_list(count))
        num_skip = count

        open (newunit=unit_num, file=trim(filename), status='old', &
              action='read', iostat=ios)
        i = 0
        do
            read (unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            trimmed_line = adjustl(line)
            if (len_trim(trimmed_line) > 0 .and. trimmed_line(1:1) /= '#') then
                i = i + 1
                if (index(trimmed_line, '#') > 0) then
                    skip_list(i) = &
                        adjustl(trimmed_line(1:index(trimmed_line, '#') - 1))
                else
                    skip_list(i) = trim(trimmed_line)
                end if
            end if
        end do
        close (unit_num)
    end subroutine load_skip_examples

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

    logical function is_skipped_example(relative_path, basename, skip_examples, &
                                        num_skip_examples) result(skip)
        character(len=*), intent(in) :: relative_path, basename
        character(len=256), intent(in) :: skip_examples(:)
        integer, intent(in) :: num_skip_examples
        character(len=256) :: normalized_entry
        character(len=256) :: normalized_relative
        character(len=256) :: normalized_basename
        integer :: i

        skip = .false.
        if (num_skip_examples <= 0) return

        normalized_relative = normalize_path_string(relative_path)
        normalized_basename = normalize_path_string(basename)

        do i = 1, num_skip_examples
            normalized_entry = normalize_path_string(skip_examples(i))
            if (len_trim(normalized_entry) == 0) cycle
            if (trim(normalized_relative) == trim(normalized_entry)) then
                skip = .true.
                return
            end if
            if (trim(normalized_basename) == trim(normalized_entry)) then
                skip = .true.
                return
            end if
        end do
    end function is_skipped_example

    subroutine test_examples_by_extension(examples_dir, extension, fortfront_exe, &
                                          temp_dir, test_count, pass_count, &
                                              fail_count, &
                                              & skip_count, &
                                          xfail_count, xpass_count, is_windows, &
                                          expected_failures, num_expected_failures, &
                                          skip_examples, num_skip_examples)
        character(len=*), intent(in) :: examples_dir, extension, fortfront_exe
        character(len=*), intent(in) :: temp_dir
        integer, intent(inout) :: test_count, pass_count, fail_count, skip_count
        integer, intent(inout) :: xfail_count, xpass_count
        logical, intent(in) :: is_windows
        character(len=256), intent(in) :: expected_failures(:)
        integer, intent(in) :: num_expected_failures
        character(len=256), intent(in) :: skip_examples(:)
        integer, intent(in) :: num_skip_examples

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
                                     expected_failures, &
                                     num_expected_failures, skip_examples, &
                                     num_skip_examples)
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

    pure function extract_relative_example_path(filepath) result(relative)
        character(len=*), intent(in) :: filepath
        character(len=256) :: relative
        character(len=:), allocatable :: trimmed
        integer :: pos
        integer :: i

        relative = ''
        if (len_trim(filepath) == 0) return

        trimmed = adjustl(trim(filepath))
        do i = 1, len(trimmed)
            if (trimmed(i:i) == '\\') trimmed(i:i) = '/'
            select case (trimmed(i:i))
            case ('A':'Z')
                trimmed(i:i) = achar(iachar(trimmed(i:i)) + 32)
            end select
        end do
        pos = index(trimmed, 'examples/')

        if (pos > 0) then
            if (pos + len('examples/') <= len(trimmed)) then
                relative = trimmed(pos + len('examples/'):)
            else
                relative = ''
            end if
        else
            relative = trimmed
        end if

        relative = adjustl(relative)
        if (len_trim(relative) == 0) then
            relative = extract_example_basename(filepath)
        end if

        do i = 1, len(relative)
            if (relative(i:i) == '\\') relative(i:i) = '/'
        end do

        relative = trim(relative)
        relative = adjustl(relative)
    end function extract_relative_example_path

    pure function normalize_path_string(value) result(normalized)
        character(len=*), intent(in) :: value
        character(len=256) :: normalized
        integer :: i

        normalized = adjustl(trim(value))
        do i = 1, len(normalized)
            if (normalized(i:i) == '\\') normalized(i:i) = '/'
        end do
        normalized = trim(normalized)
        normalized = adjustl(normalized)
    end function normalize_path_string

    subroutine test_single_example(filepath, fortfront_exe, temp_dir, test_count, &
                                   pass_count, &
                                   fail_count, skip_count, xfail_count, xpass_count, &
                                   is_windows, expected_failures, &
                                   num_expected_failures, skip_examples, &
                                   num_skip_examples)
        character(len=*), intent(in) :: filepath, fortfront_exe, temp_dir
        integer, intent(inout) :: test_count, pass_count, fail_count, skip_count
        integer, intent(inout) :: xfail_count, xpass_count
        logical, intent(in) :: is_windows
        character(len=256), intent(in) :: expected_failures(:)
        integer, intent(in) :: num_expected_failures
        character(len=256), intent(in) :: skip_examples(:)
        integer, intent(in) :: num_skip_examples

        character(len=:), allocatable :: output_file, error_file
        character(len=256) :: basename_str
        character(len=256) :: relative_path
        character(len=1) :: sep
        logical :: has_error, has_unparsed, has_warning, file_exists, expect_fail
        logical :: is_f90_roundtrip
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

        module_dir = get_module_directory(fortfront_exe)

        call run_transform_and_scan(filepath, fortfront_exe, output_file, &
                                    error_file, is_windows, is_f90_roundtrip, &
                                    has_error, has_unparsed, &
                                    has_warning)

        if (.not. has_error .and. .not. has_unparsed) then
            if (.not. compile_generated_output(output_file, module_dir, temp_dir, &
                                               is_windows)) then
                has_error = .true.
            end if
        end if

        expect_fail = is_expected_failure(trim(basename_str), expected_failures, &
                                          num_expected_failures)
        test_count = test_count + 1

        call finalize_example_result(trim(basename_str), output_file, error_file, &
                                     has_error, has_unparsed, has_warning, &
                                     expect_fail, pass_count, fail_count, &
                                     xfail_count, xpass_count, is_f90_roundtrip)

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
            command = 'cmd /C "' // trim(exe_arg) // ' ' // trim(input_arg) // &
                      ' > ' // trim(output_arg) // ' 2> ' // trim(error_arg) // '"'
        else
            command = trim(exe_arg) // ' ' // trim(input_arg) // &
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
        compile_generated_output = (exit_code == 0)
    end function compile_generated_output

    subroutine finalize_example_result(name, output_file, error_file, has_error, &
                                       has_unparsed, has_warning, expect_fail, &
                                       pass_count, fail_count, xfail_count, &
                                       xpass_count, &
                                       is_roundtrip)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: output_file, error_file
        logical, intent(in) :: has_error, has_unparsed, has_warning, expect_fail
        logical, intent(in) :: is_roundtrip
        integer, intent(inout) :: pass_count, fail_count, xfail_count, xpass_count

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

        call fallback_module_dir_search(module_dir)

        if (len_trim(module_dir) == 0) then
            sep = path_separator_for('fortfront_modules')
            if (module_directory_has_module('fortfront_modules', sep)) then
                module_dir = 'fortfront_modules'
            end if
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
            module_directory_has_module = .true.
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

    subroutine fallback_module_dir_search(module_dir)
        character(len=:), allocatable, intent(inout) :: module_dir
        character(len=256) :: search_file
        integer :: exit_code, unit_num, ios
        character(len=512) :: line
        integer :: sep_pos
        logical :: is_win

        if (len_trim(module_dir) > 0) return

        is_win = check_if_windows()
        search_file = 'fortfront_module_search.txt'

        if (is_win) then
            call execute_command_line('cmd /C "dir /s /b fortfront.mod > '// &
                                      trim(search_file)//' 2>nul"', &
                                      exitstat=exit_code)
        else
            call execute_command_line( &
                'find build -name "fortfront.mod" -print -quit > '// &
                trim(search_file)//' 2>/dev/null', exitstat=exit_code)
        end if

        if (exit_code /= 0) then
            call cleanup_file(search_file)
            return
        end if

        open (newunit=unit_num, file=trim(search_file), status='old', action='read', &
              iostat=ios)
        if (ios /= 0) then
            call cleanup_file(search_file)
            return
        end if

        read (unit_num, '(A)', iostat=ios) line
        close (unit_num)
        call cleanup_file(search_file)

        if (ios /= 0) return
        if (len_trim(line) == 0) return

        sep_pos = find_last_separator(trim(line))
        if (sep_pos > 0) then
            module_dir = trim(line(1:sep_pos - 1))
        else
            module_dir = directory_from_path(trim(line))
        end if
    end subroutine fallback_module_dir_search

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

    pure function build_compile_command(output_file, module_dir, temp_dir, &
                                        is_windows) &
        result(command)
        character(len=*), intent(in) :: output_file
        character(len=*), intent(in) :: module_dir, temp_dir
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: command
        character(len=:), allocatable :: module_arg, output_arg, temp_arg

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

        if (len_trim(temp_dir) > 0) then
            temp_arg = quote_for_shell(temp_dir, is_windows, &
                                       escape_for_cmd=is_windows)
            if (len_trim(temp_arg) > 0) then
                command = command // '-J ' // temp_arg // ' '
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

        if (len_trim(path) == 0) then
            argument = ''
        else if (is_windows .and. needs_cmd_escape) then
            argument = '""' // trim(path) // '""'
        else
            argument = '"' // trim(path) // '"'
        end if
    end function quote_for_shell

    subroutine create_temp_directory(temp_dir, is_windows)
        character(len=:), allocatable, intent(out) :: temp_dir
        logical, intent(in) :: is_windows
        character(len=256) :: base_temp
        integer :: ios
        character(len=:), allocatable :: mkdir_cmd

        if (is_windows) then
            call get_environment_variable('TEMP', base_temp, status=ios)
            if (ios /= 0) base_temp = '.'
            temp_dir = trim(base_temp) // '\fortfront_test'
            mkdir_cmd = 'cmd /C "if not exist "' // trim(temp_dir) // '" mkdir "' // &
                        trim(temp_dir) // '""'
        else
            base_temp = '/tmp'
            temp_dir = trim(base_temp) // '/fortfront_test'
            mkdir_cmd = 'mkdir -p "' // trim(temp_dir) // '"'
        end if

        call execute_command_line(trim(mkdir_cmd), exitstat=ios)
        if (ios /= 0) temp_dir = ''
    end subroutine create_temp_directory

    subroutine cleanup_temp_directory(temp_dir, is_windows)
        character(len=*), intent(in) :: temp_dir
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: rm_cmd
        integer :: ios

        if (len_trim(temp_dir) == 0) return

        if (is_windows) then
            rm_cmd = 'cmd /C "rmdir /S /Q "' // trim(temp_dir) // '""'
        else
            rm_cmd = 'rm -rf "' // trim(temp_dir) // '"'
        end if

        call execute_command_line(trim(rm_cmd), exitstat=ios)
    end subroutine cleanup_temp_directory

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

end program test_all_examples
