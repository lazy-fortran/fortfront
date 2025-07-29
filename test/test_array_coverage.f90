program test_array_coverage
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Array Coverage Tests ==='
    print *

    if (.not. test_array_literal_codegen()) all_passed = .false.
    if (.not. test_empty_array_allocatable()) all_passed = .false.
    if (.not. test_array_type_inference()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All array coverage tests passed!'
        stop 0
    else
        print *, 'Some array coverage tests failed!'
        stop 1
    end if

contains

    logical function test_array_literal_codegen()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_array_literal_codegen = .true.
        print *, 'Testing array literal code generation...'

        ! Create test input
        input_file = 'test_arr_lit.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'arr = [10, 20, 30]'
        close (unit)

        ! Compile
        output_file = 'test_arr_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) == 0) then
            print *, '  PASS: Array literal compiled successfully'
        else
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_array_literal_codegen = .false.
        end if

    end function test_array_literal_codegen

    logical function test_empty_array_allocatable()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_empty_array_allocatable = .true.
        print *, 'Testing empty array allocatable generation...'

        ! Create test with empty array
        input_file = 'test_empty.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'empty = []'
        close (unit)

        ! Compile
        output_file = 'test_empty_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) == 0) then
            print *, '  PASS: Empty array compiled successfully'
        else
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_empty_array_allocatable = .false.
        end if

    end function test_empty_array_allocatable

    logical function test_array_type_inference()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_array_type_inference = .true.
        print *, 'Testing array type inference...'

        ! Create test with mixed types
        input_file = 'test_types.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'int_arr = [1, 2, 3]'
        write (unit, '(a)') 'real_arr = [1.0, 2.0, 3.0]'
        write (unit, '(a)') 'char_arr = ["a", "b", "c"]'
        close (unit)

        ! Compile
        output_file = 'test_types_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (len_trim(error_msg) == 0) then
            print *, '  PASS: Mixed type arrays compiled successfully'
        else
            print *, '  FAIL: Compilation error:', trim(error_msg)
            test_array_type_inference = .false.
        end if

    end function test_array_type_inference

end program test_array_coverage
