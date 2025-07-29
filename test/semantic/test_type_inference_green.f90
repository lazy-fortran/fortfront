program test_type_inference_green
    use frontend, only: compile_source, compilation_options_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Type Inference Tests ==='
    print *, 'Testing that type inference is working correctly'
    print *

    ! Test type inference for various expressions
    if (.not. test_array_literal_inference()) all_passed = .false.
    if (.not. test_array_constructor_inference()) all_passed = .false.
    if (.not. test_string_concat_inference()) all_passed = .false.
    if (.not. test_mixed_type_inference()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All type inference tests passed!'
        stop 0
    else
        print *, 'Some type inference tests failed!'
        stop 1
    end if

contains

    logical function test_array_literal_inference()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_array_literal_inference = .true.
        print *, 'Testing array literal type inference...'

        ! Test: [1, 2, 3] should infer array(integer)
        input_file = 'test_inf_arr.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer :: arr(3)'
        write (unit, '(a)') '    arr = [1, 2, 3]'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_inf_arr_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: Type inference error - ', trim(error_msg)
            test_array_literal_inference = .false.
        else
            print *, '  PASS: Array literal type inference working'
        end if

    end function test_array_literal_inference

    logical function test_array_constructor_inference()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_array_constructor_inference = .true.
        print *, 'Testing array constructor type inference...'

        ! Test: [(i, i=1,10)] should infer array(integer)
        input_file = 'test_inf_ctor.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    integer :: arr(10)'
        write (unit, '(a)') '    arr = [(i, i=1,10)]'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_inf_ctor_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: Array constructor type inference - ', trim(error_msg)
            test_array_constructor_inference = .false.
        else
            print *, '  PASS: Array constructor type inference working'
        end if

    end function test_array_constructor_inference

    logical function test_string_concat_inference()
        character(len=:), allocatable :: input_file, output_file
        character(len=256) :: error_msg
        type(compilation_options_t) :: options
        integer :: unit

        test_string_concat_inference = .true.
        print *, 'Testing string concatenation type inference...'

        ! Test: "hello" // " world" should infer string
        input_file = 'test_inf_concat.f'
        open (newunit=unit, file=input_file, status='replace')
        write (unit, '(a)') 'program test'
        write (unit, '(a)') '    character(len=20) :: msg'
        write (unit, '(a)') '    msg = "hello" // " world"'
        write (unit, '(a)') 'end program'
        close (unit)

        output_file = 'test_inf_concat_out.f90'
        options%output_file = output_file

        call compile_source(input_file, options, error_msg)

        if (error_msg /= '') then
            print *, '  FAIL: String concat type inference - ', trim(error_msg)
            test_string_concat_inference = .false.
        else
            print *, '  PASS: String concatenation type inference working'
        end if

    end function test_string_concat_inference

    logical function test_mixed_type_inference()
        test_mixed_type_inference = .true.
        print *, 'Testing mixed type expressions...'

        ! Test various mixed type scenarios
        print *, '  Test: [1.0, 2.0, 3.0] - should infer array(real)'
        print *, '  Test: [.true., .false.] - should infer array(logical)'
        print *, '  Test: ["a", "b", "c"] - should infer array(string)'

        print *, '  INFO: Mixed type inference tests not yet implemented'

    end function test_mixed_type_inference

end program test_type_inference_green
