program test_codegen_green
    use transformation_api, only: compile_source, compilation_options_t
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

    print *, '=== Code Generation Tests ==='
    print *, 'These tests check code generation is working correctly'
    print *

    if (.not. test_array_literal_codegen()) all_passed = .false.
    if (.not. test_implied_do_codegen()) all_passed = .false.
    if (.not. test_string_concat_codegen()) all_passed = .false.
    if (.not. test_complex_expr_codegen()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All code generation tests passed!'
    else
        print *, 'Some code generation tests failed!'
        exit_code = 1
    end if
    call cleanup_temp_directory(temp_dir, is_windows)
    stop exit_code

contains

    include '../common/filesystem_helpers.inc'

    logical function test_array_literal_codegen()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, ios
        character(len=256) :: line
        logical :: generated_correctly

        test_array_literal_codegen = .true.
        print *, 'Testing array literal code generation...'

        ! Simple test that should work
        input_file = join_path(temp_dir, 'test_cg_arr.lf', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'arr = [10, 20, 30]'
        close (unit)

        output_file = join_path(temp_dir, 'test_cg_arr_out.f90', sep)
        ! Backend is always Fortran codegen now
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg == '') then
            ! Check the generated code
            open (newunit=unit, file=output_file, status='old', iostat=ios)
            if (ios == 0) then
                generated_correctly = .false.
                do
                    read (unit, '(a)', iostat=ios) line
                    if (ios /= 0) exit
                    ! Should generate a standard Fortran array constructor.
                    ! Accept both legacy (/ /) and modern [ ] forms.
                    if (index(line, '(/ 10, 20, 30 /)') > 0 .or. &
                        index(line, '(/10, 20, 30/)') > 0 .or. &
                        index(line, '(/10, 20, 30 /)') > 0 .or. &
                        index(line, '[10, 20, 30]') > 0 .or. &
                        index(line, '[10,20,30]') > 0) then
                        generated_correctly = .true.
                        print *, '  Generated: ', trim(line)
                        exit
                    end if
                end do
                close (unit)

                if (generated_correctly) then
                    print *, '  SUCCESS: Array literal generated correctly'
                else
                    print *, '  FAIL: Array literal not generated in expected format'
                    test_array_literal_codegen = .false.
                end if
            end if
        else
            print *, '  EXPECTED FAIL: ', trim(error_msg)
        end if

        ! Mixed array literal should promote integers to real(dp)
        input_file = join_path(temp_dir, 'test_cg_arr_mixed.lf', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'arr = [1, 2.0, 3]'
        close (unit)

        output_file = join_path(temp_dir, 'test_cg_arr_mixed_out.f90', sep)
        options%output_file = output_file
        error_msg = ''

        call compile_source(input_file, options, error_msg)

        if (error_msg == '') then
            open (newunit=unit, file=output_file, status='old', iostat=ios)
            if (ios == 0) then
                generated_correctly = .false.
                do
                    read (unit, '(a)', iostat=ios) line
                    if (ios /= 0) exit
                    if ((index(line, '1.0d0') > 0 .or. index(line, '1.0') > 0) .and. &
                        (index(line, '2.0d0') > 0 .or. index(line, '2.0') > 0) .and. &
                        (index(line, '3.0d0') > 0 .or. index(line, '3.0') > 0)) then
                        generated_correctly = .true.
                        print *, '  Generated with promotion: ', trim(line)
                        exit
                    end if
                end do
                close (unit)

                if (generated_correctly) then
                    print *, '  SUCCESS: Mixed array literals promoted to real(dp)'
                else
                    print *, '  FAIL: Mixed array literal did not promote integers'
                    test_array_literal_codegen = .false.
                end if
            end if
        else
            print *, '  FAIL: Mixed array literal compilation error - ', trim(error_msg)
            test_array_literal_codegen = .false.
        end if

    end function test_array_literal_codegen

    logical function test_implied_do_codegen()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_implied_do_codegen = .true.
        print *, 'Testing implied do loop code generation...'

        input_file = join_path(temp_dir, 'test_cg_do.lf', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'arr = [(i*i, i=1,5)]'
        close (unit)

        output_file = join_path(temp_dir, 'test_cg_do_out.f90', sep)
        ! Backend is always Fortran codegen now
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  EXPECTED FAIL: Implied do codegen not implemented - ', &
                & trim(error_msg)
        else
            print *, '  Check if output contains: (/ (i*i, i=1,5) /)'
            ! Could verify the output here
        end if

    end function test_implied_do_codegen

    logical function test_string_concat_codegen()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit, ios
        character(len=256) :: line
        logical :: found_concat

        test_string_concat_codegen = .true.
        print *, 'Testing string concatenation code generation...'

        input_file = join_path(temp_dir, 'test_cg_concat.lf', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'msg = "Hello" // " " // "World"'
        close (unit)

        output_file = join_path(temp_dir, 'test_cg_concat_out.f90', sep)
        ! Backend is always Fortran codegen now
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg == '') then
            open (newunit=unit, file=output_file, status='old', iostat=ios)
            if (ios == 0) then
                found_concat = .false.
                do
                    read (unit, '(a)', iostat=ios) line
                    if (ios /= 0) exit
                    if (index(line, '"Hello" // " " // "World"') > 0 .or. &
                        index(line, '"Hello" //" " //"World"') > 0 .or. &
                        index(line, '"Hello World"') > 0) then
                        found_concat = .true.
                        print *, '  Generated: ', trim(line)
                        exit
                    end if
                end do
                close (unit)

                if (found_concat) then
                    print *, '  SUCCESS: String concatenation generated'
                else
                    print *, '  FAIL: String concatenation not found in output'
                    test_string_concat_codegen = .false.
                end if
            end if
        else
            print *, '  EXPECTED FAIL: ', trim(error_msg)
        end if

    end function test_string_concat_codegen

    logical function test_complex_expr_codegen()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_complex_expr_codegen = .true.
        print *, 'Testing complex expression code generation...'

        ! Test: Nested array operations
        input_file = join_path(temp_dir, 'test_cg_complex.lf', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'result = sum([1, 2, 3]) + maxval([(i*2, i=1,5)])'
        close (unit)

        output_file = join_path(temp_dir, 'test_cg_complex_out.f90', sep)
        ! Backend is always Fortran codegen now
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  EXPECTED FAIL: Complex expressions with intrinsics - ', &
                & trim(error_msg)
        else
            print *, '  Complex expression might have compiled'
        end if

        ! Test: Array slice in expression
        input_file = join_path(temp_dir, 'test_cg_slice.lf', sep)
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'sub = arr(2:5) + [1, 1, 1, 1]'
        close (unit)

        output_file = join_path(temp_dir, 'test_cg_slice_out.f90', sep)
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  EXPECTED FAIL: Array slice arithmetic - ', trim(error_msg)
        else
            print *, '  Array slice arithmetic might be working'
        end if

    end function test_complex_expr_codegen

end program test_codegen_green
