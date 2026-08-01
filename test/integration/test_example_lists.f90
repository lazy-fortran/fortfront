module test_example_lists
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private
    public :: load_example_list
    public :: load_skip_examples
    public :: is_expected_failure
    public :: assert_skip_path_handling
    public :: example_list_contains
    public :: is_skipped_example
    public :: is_analysis_only_example
    public :: is_expected_diagnostic

contains

    include '../common/filesystem_helpers.inc'

    subroutine load_example_list(filename, entries, num_entries)
        character(len=*), intent(in) :: filename
        character(len=256), allocatable, intent(out) :: entries(:)
        integer, intent(out) :: num_entries
        integer :: unit_num, ios, count, i
        character(len=256) :: line, trimmed_line
        logical :: file_exists

        num_entries = 0
        inquire (file=trim(filename), exist=file_exists)
        if (.not. file_exists) then
            allocate (entries(0))
            return
        end if

        open (newunit=unit_num, file=trim(filename), status='old', &
            action='read', iostat=ios)
        if (ios /= 0) then
            allocate (entries(0))
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

        allocate (entries(count))
        num_entries = count

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
                    entries(i) = adjustl(trimmed_line(1:index(trimmed_line, '#') - 1))
                else
                    entries(i) = trim(trimmed_line)
                end if
            end if
        end do
        close (unit_num)
    end subroutine load_example_list

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

    subroutine assert_skip_path_handling()
        character(len=256) :: windows_path
        character(len=256) :: posix_path
        character(len=256) :: relative
        character(len=256) :: sample_skip(1)
        logical :: is_skip

        windows_path = 'C:\work\fortfront\examples\lf\api_complex_transform.lf'
        posix_path = '/work/fortfront/examples/lf/api_complex_transform.lf'
        sample_skip = 'lf/api_complex_transform.lf'

        relative = extract_relative_example_path(windows_path)
        if (trim(relative) /= 'lf/api_complex_transform.lf') then
            write (error_unit, '(A)') &
                'ERROR: Windows path normalization failed in test_all_examples'
            stop 1
        end if

        relative = extract_relative_example_path(posix_path)
        if (trim(relative) /= 'lf/api_complex_transform.lf') then
            write (error_unit, '(A)') &
                'ERROR: POSIX path normalization failed in test_all_examples'
            stop 1
        end if

        is_skip = is_skipped_example('lf/api_complex_transform.lf', &
            'api_complex_transform.lf', sample_skip, 1)
        if (.not. is_skip) then
            write (error_unit, '(A)') &
                'ERROR: Skip list matching failed for normalized path'
            stop 1
        end if
    end subroutine assert_skip_path_handling

    pure logical function example_list_contains(relative_path, basename, entries, &
            num_entries) result(is_listed)
        character(len=*), intent(in) :: relative_path, basename
        character(len=256), intent(in) :: entries(:)
        integer, intent(in) :: num_entries
        character(len=256) :: normalized_entry
        character(len=256) :: normalized_relative
        character(len=256) :: normalized_basename
        integer :: i

        is_listed = .false.
        if (num_entries <= 0) return

        normalized_relative = normalize_path_string(relative_path)
        normalized_basename = normalize_path_string(basename)

        do i = 1, num_entries
            normalized_entry = normalize_path_string(entries(i))
            if (len_trim(normalized_entry) == 0) cycle
            if (trim(normalized_relative) == trim(normalized_entry)) then
                is_listed = .true.
                return
            end if
            if (trim(normalized_basename) == trim(normalized_entry)) then
                is_listed = .true.
                return
            end if
        end do
    end function example_list_contains

    logical function is_skipped_example(relative_path, basename, skip_examples, &
            num_skip_examples) result(skip)
        character(len=*), intent(in) :: relative_path, basename
        character(len=256), intent(in) :: skip_examples(:)
        integer, intent(in) :: num_skip_examples

        skip = example_list_contains(relative_path, basename, skip_examples, &
            num_skip_examples)
    end function is_skipped_example

    logical function is_analysis_only_example(relative_path, basename, &
            analysis_only_examples, &
            num_analysis_only_examples) &
            result(is_analysis_only)
        character(len=*), intent(in) :: relative_path, basename
        character(len=256), intent(in) :: analysis_only_examples(:)
        integer, intent(in) :: num_analysis_only_examples

        is_analysis_only = example_list_contains(relative_path, basename, &
            analysis_only_examples, &
            num_analysis_only_examples)
    end function is_analysis_only_example

    logical function is_expected_diagnostic(relative_path, basename, &
            diagnostic_examples, &
            num_diagnostic_examples) &
            result(expect_diagnostic)
        character(len=*), intent(in) :: relative_path, basename
        character(len=256), intent(in) :: diagnostic_examples(:)
        integer, intent(in) :: num_diagnostic_examples

        expect_diagnostic = example_list_contains(relative_path, basename, &
            diagnostic_examples, &
            num_diagnostic_examples)
    end function is_expected_diagnostic

end module test_example_lists
