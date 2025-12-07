program test_standard_fixtures
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
    use test_example_lists, only: load_example_list, is_expected_failure
    use test_filesystem_helpers, only: check_if_windows, cleanup_file, &
                                       extract_example_basename
    use test_roundtrip_core, only: roundtrip_result_t, run_roundtrip_test
    implicit none

    character(len=*), parameter :: STANDARD_REPO_URL = &
                                   'https://github.com/lazy-fortran/standard.git'
    character(len=*), parameter :: STANDARD_REPO_DIR = 'build/standard'
    character(len=*), parameter :: FIXTURES_DIR = &
                                   'build/standard/tests/fixtures'
    character(len=*), parameter :: XFAIL_FILE = &
                                   'examples/standard_fixtures_xfail.txt'

    integer :: test_count, pass_count, fail_count, skip_count
    integer :: xfail_count, xpass_count
    logical :: is_windows
    character(len=256), allocatable :: expected_failures(:)
    integer :: num_expected_failures
    real(dp) :: success_rate

    test_count = 0
    pass_count = 0
    fail_count = 0
    skip_count = 0
    xfail_count = 0
    xpass_count = 0

    is_windows = check_if_windows()

    print *, "=== Standard Fixtures Roundtrip Test ==="
    print *, ""
    print *, "Testing lazy-fortran/standard fixtures:"
    print *, "  - Parse -> Emit -> Reparse -> Re-emit (text comparison)"
    print *, ""
    print *, "NOTE: gfortran compilation skipped for grammar test fixtures"
    print *, "      (many fixtures are grammar fragments, not valid programs)"
    print *, ""

    call ensure_standard_repo_cloned(is_windows)

    call load_example_list(XFAIL_FILE, expected_failures, num_expected_failures)
    print *, "Loaded ", num_expected_failures, " expected failures from xfail list"
    print *, ""

    call test_fixtures_by_standard(FIXTURES_DIR, 'Fortran90', is_windows, &
                                   expected_failures, num_expected_failures, &
                                   test_count, pass_count, fail_count, &
                                   skip_count, xfail_count, xpass_count)

    call test_fixtures_by_standard(FIXTURES_DIR, 'Fortran95', is_windows, &
                                   expected_failures, num_expected_failures, &
                                   test_count, pass_count, fail_count, &
                                   skip_count, xfail_count, xpass_count)

    call test_fixtures_by_standard(FIXTURES_DIR, 'Fortran2003', is_windows, &
                                   expected_failures, num_expected_failures, &
                                   test_count, pass_count, fail_count, &
                                   skip_count, xfail_count, xpass_count)

    call test_fixtures_by_standard(FIXTURES_DIR, 'Fortran2008', is_windows, &
                                   expected_failures, num_expected_failures, &
                                   test_count, pass_count, fail_count, &
                                   skip_count, xfail_count, xpass_count)

    call test_fixtures_by_standard(FIXTURES_DIR, 'Fortran2018', is_windows, &
                                   expected_failures, num_expected_failures, &
                                   test_count, pass_count, fail_count, &
                                   skip_count, xfail_count, xpass_count)

    print *, ""
    print *, "=== Test Summary ==="
    write (*, '(A,I0)') "Total fixtures tested: ", test_count
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
        print *, "Please update ", XFAIL_FILE
    end if

    print *, ""

    if (fail_count > 0) then
        print *, "FAILURE: Some fixtures did not roundtrip correctly"
        print *, "This indicates parser/codegen issues with standard Fortran"
        call exit(1)
    else if (xpass_count > 0) then
        print *, "FAILURE: Unexpected passes detected"
        print *, "Update ", XFAIL_FILE, " to remove fixed fixtures"
        call exit(1)
    else if (test_count == 0) then
        print *, "WARNING: No fixtures were tested"
        print *, "Standard repo may not be cloned properly"
        call exit(0)
    else
        print *, "SUCCESS: All fixtures behaved as expected"
        call exit(0)
    end if

contains

    subroutine ensure_standard_repo_cloned(is_windows)
        logical, intent(in) :: is_windows
        logical :: dir_exists
        integer :: exit_code
        character(len=512) :: clone_cmd

        inquire (file=FIXTURES_DIR//'/Fortran90', exist=dir_exists)
        if (dir_exists) then
            print *, "Standard repo already cloned at ", STANDARD_REPO_DIR
            return
        end if

        print *, "Cloning lazy-fortran/standard to ", STANDARD_REPO_DIR, "..."

        if (is_windows) then
            clone_cmd = 'git clone --depth 1 ' // STANDARD_REPO_URL // &
                        ' ' // STANDARD_REPO_DIR
        else
            clone_cmd = 'git clone --depth 1 ' // STANDARD_REPO_URL // &
                        ' ' // STANDARD_REPO_DIR // ' 2>&1'
        end if

        call execute_command_line(trim(clone_cmd), exitstat=exit_code)
        if (exit_code /= 0) then
            print *, "ERROR: Failed to clone standard repo"
            print *, "Check network access to GitHub"
            call exit(1)
        end if

        print *, "Clone complete."
        print *, ""
    end subroutine ensure_standard_repo_cloned

    subroutine test_fixtures_by_standard(fixtures_base, standard_name, is_windows, &
                                         expected_failures, num_expected_failures, &
                                         test_count, pass_count, fail_count, &
                                         skip_count, xfail_count, xpass_count)
        character(len=*), intent(in) :: fixtures_base, standard_name
        logical, intent(in) :: is_windows
        character(len=256), intent(in) :: expected_failures(:)
        integer, intent(in) :: num_expected_failures
        integer, intent(inout) :: test_count, pass_count, fail_count
        integer, intent(inout) :: skip_count, xfail_count, xpass_count

        character(len=512) :: standard_dir, list_file, find_cmd
        integer :: unit_num, ios
        character(len=512) :: line
        logical :: dir_exists

        standard_dir = trim(fixtures_base) // '/' // trim(standard_name)
        inquire (file=trim(standard_dir), exist=dir_exists)
        if (.not. dir_exists) then
            print *, "Skipping ", trim(standard_name), " (directory not found)"
            return
        end if

        print *, "=== Testing ", trim(standard_name), " fixtures ==="

        list_file = 'build/standard_fixtures_' // trim(standard_name) // '.txt'

        if (is_windows) then
            find_cmd = 'cmd /C "dir /B /S ' // trim(standard_dir) // &
                       '\*.f90 > ' // trim(list_file) // ' 2>nul"'
        else
            find_cmd = 'find ' // trim(standard_dir) // &
                       ' -name "*.f90" -type f > ' // trim(list_file) // &
                       ' 2>/dev/null || true'
        end if

        ios = 0
        call execute_command_line(trim(find_cmd), exitstat=ios)

        open (newunit=unit_num, file=trim(list_file), status='old', &
              action='read', iostat=ios)
        if (ios /= 0) then
            print *, "  No .f90 fixtures found"
            call cleanup_file(list_file)
            return
        end if

        do
            read (unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle

            call test_single_fixture(trim(line), is_windows, &
                                     expected_failures, num_expected_failures, &
                                     test_count, pass_count, fail_count, &
                                     skip_count, xfail_count, xpass_count)
        end do

        close (unit_num)
        call cleanup_file(list_file)

        print *, ""
    end subroutine test_fixtures_by_standard

    subroutine test_single_fixture(filepath, is_windows, expected_failures, &
                                   num_expected_failures, test_count, pass_count, &
                                   fail_count, skip_count, xfail_count, xpass_count)
        character(len=*), intent(in) :: filepath
        logical, intent(in) :: is_windows
        character(len=256), intent(in) :: expected_failures(:)
        integer, intent(in) :: num_expected_failures
        integer, intent(inout) :: test_count, pass_count, fail_count
        integer, intent(inout) :: skip_count, xfail_count, xpass_count

        character(len=256) :: basename_str
        character(len=:), allocatable :: source
        type(roundtrip_result_t) :: result
        logical :: expect_fail, has_error
        logical :: file_exists

        basename_str = extract_example_basename(filepath)
        write (*, '(A)', advance='no') "  " // trim(basename_str) // " ... "

        inquire (file=trim(filepath), exist=file_exists)
        if (.not. file_exists) then
            print *, "SKIP (file not found)"
            skip_count = skip_count + 1
            return
        end if

        call read_fixture_file(filepath, source)
        if (.not. allocated(source) .or. len_trim(source) == 0) then
            print *, "SKIP (could not read file)"
            skip_count = skip_count + 1
            return
        end if

        expect_fail = is_expected_failure(trim(basename_str), expected_failures, &
                                          num_expected_failures)
        test_count = test_count + 1

        call run_roundtrip_test(source, result, skip_compile=.true., &
                                is_windows=is_windows)

        has_error = .not. result%success
        call finalize_fixture_result(basename_str, expect_fail, has_error, &
                                     trim(result%error_message), result, &
                                     pass_count, fail_count, xfail_count, &
                                     xpass_count)
    end subroutine test_single_fixture

    subroutine finalize_fixture_result(name, expect_fail, has_error, error_detail, &
                                       result, pass_count, fail_count, &
                                       xfail_count, xpass_count)
        character(len=*), intent(in) :: name
        logical, intent(in) :: expect_fail, has_error
        character(len=*), intent(in) :: error_detail
        type(roundtrip_result_t), intent(in) :: result
        integer, intent(inout) :: pass_count, fail_count, xfail_count, xpass_count

        if (has_error) then
            if (expect_fail) then
                print *, "XFAIL"
                xfail_count = xfail_count + 1
            else
                if (index(trim(error_detail), 'roundtrip output differs') > 0) then
                    call print_roundtrip_difference(name, result)
                end if
                if (len_trim(error_detail) > 0) then
                    print *, "FAIL (", trim(error_detail), ")"
                else
                    print *, "FAIL"
                end if
                fail_count = fail_count + 1
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
    end subroutine finalize_fixture_result

    subroutine print_roundtrip_difference(name, result)
        character(len=*), intent(in) :: name
        type(roundtrip_result_t), intent(in) :: result
        integer :: first_len, second_len
        integer :: start_pos

        print *, "  Roundtrip difference for fixture: ", trim(name)
        first_len = len_trim(result%first_output)
        second_len = len_trim(result%second_output)
        print *, "  First output length: ", first_len
        print *, "  Second output length: ", second_len
        if (first_len > 0) then
            print *, "  First output (prefix):"
            print *, trim(result%first_output(1:min(first_len, 200)))
            print *, "  First output (full):"
            print *, trim(result%first_output)
        end if
        if (second_len > 0) then
            print *, "  Second output (prefix):"
            print *, trim(result%second_output(1:min(second_len, 200)))
            print *, "  Second output (full):"
            print *, trim(result%second_output)
        end if
        if (first_len > 0) then
            start_pos = max(1, first_len - 199)
            print *, "  First output (suffix):"
            print *, trim(result%first_output(start_pos:first_len))
        end if
        if (second_len > 0) then
            start_pos = max(1, second_len - 199)
            print *, "  Second output (suffix):"
            print *, trim(result%second_output(start_pos:second_len))
        end if
    end subroutine print_roundtrip_difference

    subroutine read_fixture_file(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit_num, iostat_val, file_size

        ! Read the entire fixture as a single stream into a deferred-length
        ! character variable. This avoids intermediate buffers and relies
        ! only on standard-conforming stream I/O (ISO/IEC 1539-1:2018,
        ! 12.6.3) and allocatable assignment semantics (10.2.1.3).

        open (newunit=unit_num, file=filepath, status='old', action='read', &
              access='stream', iostat=iostat_val)
        if (iostat_val /= 0) then
            content = ''
            return
        end if

        inquire (unit=unit_num, size=file_size)
        if (file_size <= 0) then
            close (unit_num)
            content = ''
            return
        end if

        allocate (character(len=file_size) :: content)
        read (unit_num, iostat=iostat_val) content
        close (unit_num)

        if (iostat_val /= 0) then
            content = ''
            return
        end if
    end subroutine read_fixture_file

end program test_standard_fixtures
