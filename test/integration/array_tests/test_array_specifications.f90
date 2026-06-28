program test_array_specifications
    use transformation_api, only: compile_source, compilation_options_t
    use test_filesystem_helpers, only: check_if_windows, create_temp_directory, &
        cleanup_temp_directory, join_path, &
        path_separator_for
    implicit none

    logical :: all_passed
    integer :: n_tests, n_passed
    logical :: is_windows
    character(len=:), allocatable :: temp_dir
    character(len=1) :: sep
    integer :: exit_code

    n_tests = 0
    n_passed = 0
    exit_code = 0
    is_windows = check_if_windows()
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) then
        print *, 'FAIL: could not create temporary directory'
        stop 1
    end if
    sep = path_separator_for(temp_dir)

    print *, '=== Comprehensive Array Specification Tests ==='
    print *

    if (test_dynamic_arrays()) n_passed = n_passed + 1
    n_tests = n_tests + 1

    if (test_fixed_arrays()) n_passed = n_passed + 1
    n_tests = n_tests + 1

    if (test_explicit_ranges()) n_passed = n_passed + 1
    n_tests = n_tests + 1

    if (test_computed_sizes()) n_passed = n_passed + 1
    n_tests = n_tests + 1

    if (test_multidimensional()) n_passed = n_passed + 1
    n_tests = n_tests + 1

    print *
    print *, 'Results:', n_passed, '/', n_tests, ' tests passed'

    all_passed = (n_passed == n_tests)

    if (all_passed) then
        print *, 'All array specification tests passed!'
    else
        print *, 'Some array specification tests failed!'
        exit_code = 1
    end if
    call cleanup_temp_directory(temp_dir, is_windows)
    stop exit_code

contains

    logical function test_dynamic_arrays()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_dynamic_arrays = .true.
        print *, 'Test 1: Dynamic arrays (dimension(:))'

        input_file = join_path(temp_dir, 'test_dynamic_arrays.f90', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'function one_array_dynamic(x) result(res)'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    real, dimension(:), intent(in) :: x'
        write (unit, '(a)') '    real :: res'
        write (unit, '(a)') '    res = sum(x)'
        write (unit, '(a)') 'end function one_array_dynamic'
        close (unit)

        output_file = join_path(temp_dir, 'test_dynamic_arrays_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_dynamic_arrays = .false.
        else
            print *, '  PASS: Dynamic arrays compiled successfully'
        end if

    end function test_dynamic_arrays

    logical function test_fixed_arrays()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_fixed_arrays = .true.
        print *, 'Test 2: Fixed arrays (dimension(3))'

        input_file = join_path(temp_dir, 'test_fixed_arrays.f90', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'subroutine test_fixed()'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    real, dimension(3) :: arr'
        write (unit, '(a)') '    arr = [1.0, 2.0, 3.0]'
        write (unit, '(a)') '    print *, arr'
        write (unit, '(a)') 'end subroutine test_fixed'
        close (unit)

        output_file = join_path(temp_dir, 'test_fixed_arrays_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_fixed_arrays = .false.
        else
            print *, '  PASS: Fixed arrays compiled successfully'
        end if

    end function test_fixed_arrays

    logical function test_explicit_ranges()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_explicit_ranges = .true.
        print *, 'Test 3: Explicit ranges (dimension(1:3))'

        input_file = join_path(temp_dir, 'test_explicit_ranges.f90', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'subroutine test_ranges()'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    real, dimension(1:3) :: arr'
        write (unit, '(a)') '    integer :: i'
        write (unit, '(a)') '    do i = 1, 3'
        write (unit, '(a)') '        arr(i) = real(i)'
        write (unit, '(a)') '    end do'
        write (unit, '(a)') '    print *, arr'
        write (unit, '(a)') 'end subroutine test_ranges'
        close (unit)

        output_file = join_path(temp_dir, 'test_explicit_ranges_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_explicit_ranges = .false.
        else
            print *, '  PASS: Explicit ranges compiled successfully'
        end if

    end function test_explicit_ranges

    logical function test_computed_sizes()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_computed_sizes = .true.
        print *, 'Test 4: Computed sizes (dimension(size(x)))'

        input_file = join_path(temp_dir, 'test_computed_sizes.f90', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'function two_arrays_2d_dynamic(y, x) result(res)'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    real, dimension(:), intent(in) :: x, y'
        write (unit, '(a)') '    real, dimension(size(x), size(y)) :: res'
        write (unit, '(a)') '    integer :: i, j'
        write (unit, '(a)') '    do j = 1, size(y)'
        write (unit, '(a)') '        do i = 1, size(x)'
        write (unit, '(a)') '            res(i, j) = x(i) * y(j)'
        write (unit, '(a)') '        end do'
        write (unit, '(a)') '    end do'
        write (unit, '(a)') 'end function two_arrays_2d_dynamic'
        close (unit)

        output_file = join_path(temp_dir, 'test_computed_sizes_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_computed_sizes = .false.
        else
            print *, '  PASS: Computed sizes compiled successfully'
        end if

    end function test_computed_sizes

    logical function test_multidimensional()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_multidimensional = .true.
        print *, 'Test 5: Multidimensional combinations'

        input_file = join_path(temp_dir, 'test_multidimensional.f90', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'subroutine test_multi()'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    real, dimension(3, 4) :: arr2d'
        write (unit, '(a)') '    real, dimension(:, :), allocatable :: arr_dyn'
        write (unit, '(a)') '    integer :: i, j'
        write (unit, '(a)') '    allocate(arr_dyn(2, 3))'
        write (unit, '(a)') '    do j = 1, 4'
        write (unit, '(a)') '        do i = 1, 3'
        write (unit, '(a)') '            arr2d(i, j) = real(i + j)'
        write (unit, '(a)') '        end do'
        write (unit, '(a)') '    end do'
        write (unit, '(a)') '    print *, arr2d(1, 1)'
        write (unit, '(a)') '    deallocate(arr_dyn)'
        write (unit, '(a)') 'end subroutine test_multi'
        close (unit)

        output_file = join_path(temp_dir, 'test_multidimensional_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_multidimensional = .false.
        else
            print *, '  PASS: Multidimensional combinations compiled successfully'
        end if

    end function test_multidimensional

end program test_array_specifications
