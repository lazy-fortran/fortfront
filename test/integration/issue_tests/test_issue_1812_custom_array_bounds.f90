program test_issue_1812_custom_array_bounds
    ! Test that custom array bounds are preserved correctly
    ! Issue #1812: Arrays with explicit bounds like arr(0:9) were being
    ! converted to allocatable arrays, losing the custom bounds
    use transformation_api, only: compile_source, compilation_options_t
    use test_filesystem_helpers, only: check_if_windows, create_temp_directory, &
                                       cleanup_temp_directory, join_path, &
                                       path_separator_for

    logical :: all_passed
    integer :: n_tests, n_passed
    logical :: is_windows
    character(len=:), allocatable :: temp_dir
    character(len=1) :: sep

    n_tests = 0
    n_passed = 0
    is_windows = check_if_windows()
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) error stop 'FAIL: could not create temporary directory'
    sep = path_separator_for(temp_dir)

    print *, '=== Issue 1812: Custom Array Bounds Preservation Tests ==='
    print *

    if (test_single_custom_bounds()) n_passed = n_passed + 1
    n_tests = n_tests + 1

    if (test_negative_custom_bounds()) n_passed = n_passed + 1
    n_tests = n_tests + 1

    if (test_multi_dimensional_custom_bounds()) n_passed = n_passed + 1
    n_tests = n_tests + 1

    print *
    print *, 'Results:', n_passed, '/', n_tests, ' tests passed'

    all_passed = (n_passed == n_tests)

    call cleanup_temp_directory(temp_dir, is_windows)
    if (all_passed) then
        print *, 'All custom array bounds tests passed!'
        stop 0
    else
        print *, 'Some custom array bounds tests failed!'
        stop 1
    end if

contains

    logical function test_single_custom_bounds()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat
        character(len=5000) :: file_content

        test_single_custom_bounds = .true.
        print *, 'Test 1: Single dimension custom bounds (0:9)'

        input_file = join_path(temp_dir, 'test_custom_bounds_single.f90', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test_custom_bounds_single'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    integer :: arr(0:9)'
        write (unit, '(a)') '    integer :: i'
        write (unit, '(a)') '    do i = 0, 9'
        write (unit, '(a)') '        arr(i) = i'
        write (unit, '(a)') '    end do'
        write (unit, '(a)') '    print *, arr'
        write (unit, '(a)') 'end program test_custom_bounds_single'
        close (unit)

        output_file = join_path(temp_dir, 'test_custom_bounds_single_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_single_custom_bounds = .false.
            return
        end if

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open output file'
            test_single_custom_bounds = .false.
            return
        end if

        file_content = ''
        block
            character(len=1000) :: line
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                file_content = trim(file_content) // ' ' // trim(line)
            end do
        end block
        close (unit)

        if (index(file_content, 'arr(0:9)') == 0) then
            print *, '  FAIL: Custom bounds (0:9) not preserved in output'
            test_single_custom_bounds = .false.
        else if (index(file_content, 'allocatable') > 0) then
            print *, '  FAIL: Array incorrectly marked as allocatable'
            test_single_custom_bounds = .false.
        else
            print *, '  PASS: Custom bounds (0:9) preserved correctly'
        end if

    end function test_single_custom_bounds

    logical function test_negative_custom_bounds()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat
        character(len=5000) :: file_content

        test_negative_custom_bounds = .true.
        print *, 'Test 2: Negative custom bounds (-5:5)'

        input_file = join_path(temp_dir, 'test_custom_bounds_negative.f90', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test_custom_bounds_negative'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    real :: arr(-5:5)'
        write (unit, '(a)') '    integer :: i'
        write (unit, '(a)') '    do i = -5, 5'
        write (unit, '(a)') '        arr(i) = real(i) * 0.5'
        write (unit, '(a)') '    end do'
        write (unit, '(a)') '    print *, arr'
        write (unit, '(a)') 'end program test_custom_bounds_negative'
        close (unit)

        output_file = join_path(temp_dir, 'test_custom_bounds_negative_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_negative_custom_bounds = .false.
            return
        end if

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open output file'
            test_negative_custom_bounds = .false.
            return
        end if

        file_content = ''
        block
            character(len=1000) :: line
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                file_content = trim(file_content) // ' ' // trim(line)
            end do
        end block
        close (unit)

        if (index(file_content, 'arr(-5:5)') == 0) then
            print *, '  FAIL: Custom bounds (-5:5) not preserved in output'
            test_negative_custom_bounds = .false.
        else if (index(file_content, 'allocatable') > 0) then
            print *, '  FAIL: Array incorrectly marked as allocatable'
            test_negative_custom_bounds = .false.
        else
            print *, '  PASS: Custom bounds (-5:5) preserved correctly'
        end if

    end function test_negative_custom_bounds

    logical function test_multi_dimensional_custom_bounds()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat
        character(len=5000) :: file_content

        test_multi_dimensional_custom_bounds = .true.
        print *, 'Test 3: Multi-dimensional custom bounds (2:4, 3:6)'

        input_file = join_path(temp_dir, 'test_custom_bounds_multi.f90', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test_custom_bounds_multi'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    integer :: arr(2:4, 3:6)'
        write (unit, '(a)') '    arr(2,3) = 1'
        write (unit, '(a)') '    arr(2,4) = 2'
        write (unit, '(a)') '    print *, arr(2,:)'
        write (unit, '(a)') 'end program test_custom_bounds_multi'
        close (unit)

        output_file = join_path(temp_dir, 'test_custom_bounds_multi_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_multi_dimensional_custom_bounds = .false.
            return
        end if

        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat /= 0) then
            print *, '  FAIL: Could not open output file'
            test_multi_dimensional_custom_bounds = .false.
            return
        end if

        file_content = ''
        block
            character(len=1000) :: line
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                file_content = trim(file_content) // ' ' // trim(line)
            end do
        end block
        close (unit)

        if (index(file_content, 'arr(2:4,3:6)') == 0 .and. &
            index(file_content, 'arr(2:4, 3:6)') == 0) then
            print *, '  FAIL: Custom bounds (2:4, 3:6) not preserved in output'
            test_multi_dimensional_custom_bounds = .false.
        else if (index(file_content, 'allocatable') > 0) then
            print *, '  FAIL: Array incorrectly marked as allocatable'
            test_multi_dimensional_custom_bounds = .false.
        else
            print *, '  PASS: Custom bounds (2:4, 3:6) preserved correctly'
        end if

    end function test_multi_dimensional_custom_bounds

end program test_issue_1812_custom_array_bounds
