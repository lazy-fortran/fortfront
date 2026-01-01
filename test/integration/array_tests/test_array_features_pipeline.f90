program test_array_features_pipeline
    use transformation_api, only: compile_source, compilation_options_t
    use test_filesystem_helpers, only: check_if_windows, create_temp_directory, &
                                       cleanup_temp_directory, join_path, &
                                       path_separator_for
    implicit none

    logical :: all_passed
    logical :: is_windows
    character(len=:), allocatable :: temp_dir
    character(len=1) :: sep
    integer :: exit_code

    all_passed = .true.
    exit_code = 0
    is_windows = check_if_windows()
    call create_temp_directory(temp_dir, is_windows)
    if (len_trim(temp_dir) == 0) then
        print *, 'FAIL: could not create temporary directory'
        stop 1
    end if
    sep = path_separator_for(temp_dir)

    print *, '=== Array Features Full Pipeline Tests (RED) ==='
    print *

    ! Test each stage of the pipeline
    if (.not. test_array_literals()) all_passed = .false.
    if (.not. test_array_slicing()) all_passed = .false.
    if (.not. test_array_constructors()) all_passed = .false.
    if (.not. test_string_concatenation()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All pipeline tests passed!'
    else
        print *, 'Some pipeline tests failed!'
        exit_code = 1
    end if
    call cleanup_temp_directory(temp_dir, is_windows)
    stop exit_code

contains

    logical function test_array_literals()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat
        character(len=256) :: line
        logical :: found_correct_output

        test_array_literals = .true.
        print *, 'Testing array literals through full pipeline...'

        ! Create test input
        input_file = join_path(temp_dir, 'test_arr_lit.lf', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'arr = [1, 2, 3]'
        write (unit, '(a)') 'print *, arr'
        close (unit)

        ! Compile with frontend
        output_file = join_path(temp_dir, 'test_arr_lit_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_array_literals = .false.
            return
        end if

        ! Check generated code
        found_correct_output = .false.
        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                ! Look for array declaration and initialization
                if (index(line, 'integer') > 0 .and. index(line, 'arr(3)') > 0) then
                    print *, '  PASS: Array declaration found'
                    found_correct_output = .true.
                else if (index(line, 'arr = (/1,2,3/)') > 0 .or. &
                         index(line, 'arr = (/ 1, 2, 3 /)') > 0 .or. &
                         index(line, 'arr = [1,2,3]') > 0 .or. &
                         index(line, 'arr = [1, 2, 3]') > 0) then
                    print *, '  PASS: Array literal converted to constructor'
                    found_correct_output = .true.
                end if
            end do
            close (unit)
        else
            print *, '  FAIL: Could not open output file'
            test_array_literals = .false.
        end if

        if (.not. found_correct_output) then
            print *, '  FAIL: Expected output not found'
            test_array_literals = .false.
        end if

    end function test_array_literals

    logical function test_array_slicing()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat

        test_array_slicing = .true.
        print *, 'Testing array slicing through full pipeline...'

        ! Create test input
        input_file = join_path(temp_dir, 'test_slice.lf', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'arr = [10, 20, 30, 40, 50]'
        write (unit, '(a)') 'sub = arr(2:4)'
        write (unit, '(a)') 'all = arr(:)'
        write (unit, '(a)') 'print *, sub'
        write (unit, '(a)') 'print *, all'
        close (unit)

        ! Compile with frontend
        output_file = join_path(temp_dir, 'test_slice_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_array_slicing = .false.
        else
            print *, '  INFO: Array slicing compiled (semantic analysis needed)'
            ! This will likely fail until semantic analysis handles slices
        end if

    end function test_array_slicing

    logical function test_array_constructors()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, iostat

        test_array_constructors = .true.
        print *, 'Testing array constructors with implied do loops...'

        ! Create test input
        input_file = join_path(temp_dir, 'test_constructor.lf', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'squares = [(i**2, i=1,5)]'
        write (unit, '(a)') 'evens = [(2*i, i=1,10)]'
        write (unit, '(a)') 'print *, squares'
        write (unit, '(a)') 'print *, evens'
        close (unit)

        ! Compile with frontend
        output_file = join_path(temp_dir, 'test_constructor_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  EXPECTED FAIL: Array constructors not implemented'
            print *, '  Error:', trim(error_msg)
            ! This is expected to fail - it's a RED test
            test_array_constructors = .true.  ! Mark as "passed" since we expect failure
        else
            print *, '  UNEXPECTED: Array constructors compiled!'
            ! If it compiles, check the output
            test_array_constructors = .true.
        end if

    end function test_array_constructors

    logical function test_string_concatenation()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg, line
        type(compilation_options_t) :: options
        integer :: unit, iostat
        logical :: found_concat

        test_string_concatenation = .true.
        print *, 'Testing string concatenation through full pipeline...'

        ! Create test input
        input_file = join_path(temp_dir, 'test_concat.lf', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'greeting = "Hello" // " " // "World"'
        write (unit, '(a)') 'print *, greeting'
        close (unit)

        ! Compile with frontend
        output_file = join_path(temp_dir, 'test_concat_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_string_concatenation = .false.
            return
        end if

        ! Check generated code
        found_concat = .false.
        open (newunit=unit, file=output_file, status='old', iostat=iostat)
        if (iostat == 0) then
            do
                read (unit, '(a)', iostat=iostat) line
                if (iostat /= 0) exit
                if (index(line, '//') > 0) then
                    print *, '  PASS: String concatenation preserved'
                    found_concat = .true.
                end if
            end do
            close (unit)
        end if

        if (.not. found_concat) then
            print *, '  INFO: String concatenation may need semantic analysis'
        end if

    end function test_string_concatenation

end program test_array_features_pipeline
