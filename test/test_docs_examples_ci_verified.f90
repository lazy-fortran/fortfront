program test_docs_examples_ci_verified
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use transformation_api, only: transform_with_context, transform_context_t, &
                                  INPUT_MODE_LAZY, OPERATING_MODE_INFER
    implicit none

    logical :: all_passed
    character(len=*), parameter :: EXAMPLES_F90_DIR = 'examples/f90/'
    character(len=*), parameter :: EXPECTED_CHARACTER_LENGTH = &
                                   EXAMPLES_F90_DIR // &
                                   'docs_character_length_inference_out.f90'
    character(len=*), parameter :: EXPECTED_STRING_CONCAT = &
                                   EXAMPLES_F90_DIR // &
                                   'docs_string_concatenation_out.f90'
    character(len=*), parameter :: EXPECTED_VARIABLE_LENGTH_STRINGS = &
                                   EXAMPLES_F90_DIR // &
                                   'docs_variable_length_strings_out.f90'
    character(len=*), parameter :: EXPECTED_FIXED_LENGTH_REASSIGNMENT = &
                                   EXAMPLES_F90_DIR // &
                                   'docs_fixed_length_reassignment_out.f90'
    character(len=*), parameter :: EXPECTED_CHARACTER_ARRAYS = &
                                   EXAMPLES_F90_DIR // 'docs_character_arrays_out.f90'
    character(len=*), parameter :: EXPECTED_TYPE_VALIDATION_CALC = &
                                   EXAMPLES_F90_DIR // &
                                   'docs_type_validation_calculate_out.f90'
    character(len=*), parameter :: EXPECTED_MIXED_TYPE_OPERATIONS = &
                                   EXAMPLES_F90_DIR // &
                                   'docs_mixed_type_operations_out.f90'

    all_passed = .true.

    if (.not. test_character_length_inference()) all_passed = .false.
    if (.not. test_string_concatenation()) all_passed = .false.
    if (.not. test_variable_length_strings()) all_passed = .false.
    if (.not. test_fixed_length_reassignment()) all_passed = .false.
    if (.not. test_character_arrays()) all_passed = .false.
    if (.not. test_type_validation_calculate()) all_passed = .false.
    if (.not. test_mixed_type_operations()) all_passed = .false.

    if (all_passed) then
        print *, 'PASS: docs examples are CI-verified'
        stop 0
    else
        error stop 'FAIL: docs examples are CI-verified'
    end if

contains

    include 'common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

    subroutine assert_output_matches_example(test_name, output, expected_path, passed)
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: output
        character(len=*), intent(in) :: expected_path
        logical, intent(inout) :: passed
        character(len=:), allocatable :: expected

        call read_example(expected_path, expected)
        call assert_text_equals(test_name, output, expected, expected_path, passed)
    end subroutine assert_output_matches_example

    logical function test_character_length_inference()
        type(transform_context_t) :: context
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_character_length_inference = .true.

        call read_example('examples/lf/docs_character_length_inference.lf', input)
        context%source_name = 'examples/lf/docs_character_length_inference.lf'
        context%has_filename = .true.
        context%input_mode = INPUT_MODE_LAZY
        context%operating_mode = OPERATING_MODE_INFER
        call transform_with_context(input, output, error_msg, context)

        call assert_transform_ok('docs_character_length_inference', output, &
                                 error_msg, test_character_length_inference)
        if (.not. test_character_length_inference) return

        call assert_output_matches_example('docs_character_length_inference', output, &
                                           EXPECTED_CHARACTER_LENGTH, &
                                           test_character_length_inference)
        if (.not. test_character_length_inference) return

        call assert_contains('docs_character_length_inference', output, &
                             'character(len=5) :: name', &
                             test_character_length_inference)
        call assert_contains('docs_character_length_inference', output, &
                             'name = "hello"', test_character_length_inference)
    end function test_character_length_inference

    logical function test_string_concatenation()
        type(transform_context_t) :: context
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_string_concatenation = .true.

        call read_example('examples/lf/docs_string_concatenation.lf', input)
        context%source_name = 'examples/lf/docs_string_concatenation.lf'
        context%has_filename = .true.
        context%input_mode = INPUT_MODE_LAZY
        context%operating_mode = OPERATING_MODE_INFER
        call transform_with_context(input, output, error_msg, context)

        call assert_transform_ok('docs_string_concatenation', output, error_msg, &
                                 test_string_concatenation)
        if (.not. test_string_concatenation) return

        call assert_output_matches_example('docs_string_concatenation', output, &
                                           EXPECTED_STRING_CONCAT, &
                                           test_string_concatenation)
        if (.not. test_string_concatenation) return

        call assert_contains('docs_string_concatenation', output, &
                             'character(len=11) :: message', &
                             test_string_concatenation)
        call assert_contains('docs_string_concatenation', output, &
                             'message = "hello" //" world"', &
                             test_string_concatenation)
    end function test_string_concatenation

    logical function test_variable_length_strings()
        type(transform_context_t) :: context
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_variable_length_strings = .true.

        call read_example('examples/lf/docs_variable_length_strings.lf', input)
        context%source_name = 'examples/lf/docs_variable_length_strings.lf'
        context%has_filename = .true.
        context%input_mode = INPUT_MODE_LAZY
        context%operating_mode = OPERATING_MODE_INFER
        call transform_with_context(input, output, error_msg, context)

        call assert_transform_ok('docs_variable_length_strings', output, error_msg, &
                                 test_variable_length_strings)
        if (.not. test_variable_length_strings) return

        call assert_output_matches_example('docs_variable_length_strings', output, &
                                           EXPECTED_VARIABLE_LENGTH_STRINGS, &
                                           test_variable_length_strings)
        if (.not. test_variable_length_strings) return

        call assert_contains('docs_variable_length_strings', output, &
                             'character(len=5) :: message', &
                             test_variable_length_strings)
        call assert_contains('docs_variable_length_strings', output, &
                             'message = "hello"', test_variable_length_strings)
        call assert_contains('docs_variable_length_strings', output, &
                             'message = "hi"', test_variable_length_strings)
    end function test_variable_length_strings

    logical function test_fixed_length_reassignment()
        type(transform_context_t) :: context
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_fixed_length_reassignment = .true.

        call read_example('examples/lf/docs_fixed_length_reassignment.lf', input)
        context%source_name = 'examples/lf/docs_fixed_length_reassignment.lf'
        context%has_filename = .true.
        context%input_mode = INPUT_MODE_LAZY
        context%operating_mode = OPERATING_MODE_INFER
        call transform_with_context(input, output, error_msg, context)

        call assert_transform_ok('docs_fixed_length_reassignment', output, &
                                 error_msg, test_fixed_length_reassignment)
        if (.not. test_fixed_length_reassignment) return

        call assert_output_matches_example('docs_fixed_length_reassignment', output, &
                                           EXPECTED_FIXED_LENGTH_REASSIGNMENT, &
                                           test_fixed_length_reassignment)
        if (.not. test_fixed_length_reassignment) return

        call assert_contains('docs_fixed_length_reassignment', output, &
                             'character(len=3) :: code', &
                             test_fixed_length_reassignment)
        call assert_contains('docs_fixed_length_reassignment', output, &
                             'code = "ABC"', test_fixed_length_reassignment)
        call assert_contains('docs_fixed_length_reassignment', output, &
                             'code = "XYZ"', test_fixed_length_reassignment)
    end function test_fixed_length_reassignment

    logical function test_character_arrays()
        type(transform_context_t) :: context
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_character_arrays = .true.

        call read_example('examples/lf/docs_character_arrays.lf', input)
        context%source_name = 'examples/lf/docs_character_arrays.lf'
        context%has_filename = .true.
        context%input_mode = INPUT_MODE_LAZY
        context%operating_mode = OPERATING_MODE_INFER
        call transform_with_context(input, output, error_msg, context)

        call assert_transform_ok('docs_character_arrays', output, error_msg, &
                                 test_character_arrays)
        if (.not. test_character_arrays) return

        call assert_output_matches_example('docs_character_arrays', output, &
                                           EXPECTED_CHARACTER_ARRAYS, &
                                           test_character_arrays)
        if (.not. test_character_arrays) return

        call assert_contains('docs_character_arrays', output, &
                             'character(len=7) :: names(3)', test_character_arrays)
        call assert_contains('docs_character_arrays', output, &
                             'names = ["alice  ", "bob    ", "charlie"]', &
                             test_character_arrays)
    end function test_character_arrays

    logical function test_type_validation_calculate()
        type(transform_context_t) :: context
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_type_validation_calculate = .true.

        call read_example('examples/lf/docs_type_validation_calculate.lf', input)
        context%source_name = 'examples/lf/docs_type_validation_calculate.lf'
        context%has_filename = .true.
        context%input_mode = INPUT_MODE_LAZY
        context%operating_mode = OPERATING_MODE_INFER
        call transform_with_context(input, output, error_msg, context)

        call assert_transform_ok('docs_type_validation_calculate', output, &
                                 error_msg, test_type_validation_calculate)
        if (.not. test_type_validation_calculate) return

        call assert_output_matches_example('docs_type_validation_calculate', output, &
                                           EXPECTED_TYPE_VALIDATION_CALC, &
                                           test_type_validation_calculate)
        if (.not. test_type_validation_calculate) return

        call assert_contains('docs_type_validation_calculate', output, &
                             'program main', &
                             test_type_validation_calculate)
        call assert_contains('docs_type_validation_calculate', output, &
                             'real :: val', test_type_validation_calculate)
        call assert_contains('docs_type_validation_calculate', output, &
                             'real function calculate(a, b) result(res)', &
                             test_type_validation_calculate)
        call assert_contains('docs_type_validation_calculate', output, &
                             'real, intent(in) :: a', test_type_validation_calculate)
        call assert_contains('docs_type_validation_calculate', output, &
                             'real, intent(in) :: b', test_type_validation_calculate)
        call assert_contains('docs_type_validation_calculate', output, &
                             'res = a + b', test_type_validation_calculate)
    end function test_type_validation_calculate

    logical function test_mixed_type_operations()
        type(transform_context_t) :: context
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_mixed_type_operations = .true.

        call read_example('examples/lf/docs_mixed_type_operations.lf', input)
        context%source_name = 'examples/lf/docs_mixed_type_operations.lf'
        context%has_filename = .true.
        context%input_mode = INPUT_MODE_LAZY
        context%operating_mode = OPERATING_MODE_INFER
        call transform_with_context(input, output, error_msg, context)

        call assert_transform_ok('docs_mixed_type_operations', output, error_msg, &
                                 test_mixed_type_operations)
        if (.not. test_mixed_type_operations) return

        call assert_output_matches_example('docs_mixed_type_operations', output, &
                                           EXPECTED_MIXED_TYPE_OPERATIONS, &
                                           test_mixed_type_operations)
        if (.not. test_mixed_type_operations) return

        call assert_contains('docs_mixed_type_operations', output, &
                             'program main', &
                             test_mixed_type_operations)
        call assert_contains('docs_mixed_type_operations', output, &
                             'real :: value', test_mixed_type_operations)
        call assert_contains('docs_mixed_type_operations', output, &
                             'real function mixed_calc(i, x) result(y)', &
                             test_mixed_type_operations)
        call assert_contains('docs_mixed_type_operations', output, &
                             'integer, intent(in) :: i', test_mixed_type_operations)
        call assert_contains('docs_mixed_type_operations', output, &
                             'real, intent(in) :: x', test_mixed_type_operations)
        call assert_contains('docs_mixed_type_operations', output, &
                             'y = i + x', test_mixed_type_operations)
    end function test_mixed_type_operations

    subroutine assert_transform_ok(test_name, output, error_msg, passed)
        character(len=*), intent(in) :: test_name
        character(len=:), allocatable, intent(in) :: output
        character(len=:), allocatable, intent(in) :: error_msg
        logical, intent(inout) :: passed

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: ' // trim(test_name) // &
                ' transform error: ' // trim(error_msg)
            passed = .false.
            return
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: ' // trim(test_name) // &
                ' produced no output'
            passed = .false.
        end if
    end subroutine assert_transform_ok

    subroutine assert_contains(test_name, haystack, needle, passed)
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: haystack
        character(len=*), intent(in) :: needle
        logical, intent(inout) :: passed

        if (index(haystack, needle) == 0) then
            write (error_unit, '(A)') 'FAIL: ' // trim(test_name) // &
                ' missing: ' // trim(needle)
            passed = .false.
        end if
    end subroutine assert_contains

    subroutine assert_text_equals(test_name, actual, expected, expected_path, passed)
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: expected_path
        logical, intent(inout) :: passed
        character(len=:), allocatable :: actual_norm
        character(len=:), allocatable :: expected_norm

        actual_norm = rstrip_whitespace(strip_carriage_returns(actual))
        expected_norm = rstrip_whitespace(strip_carriage_returns(expected))

        if (actual_norm /= expected_norm) then
            write (error_unit, '(A)') 'FAIL: ' // trim(test_name) // &
                ' output mismatch vs ' // trim(expected_path)
            write (error_unit, '(A)') '--- expected (' // trim(expected_path) // ') ---'
            write (error_unit, '(A)') expected_norm
            write (error_unit, '(A)') '--- actual ---'
            write (error_unit, '(A)') actual_norm
            passed = .false.
        end if
    end subroutine assert_text_equals

    pure function strip_carriage_returns(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: i
        integer :: out_len
        integer :: pos

        out_len = 0
        do i = 1, len(text)
            if (text(i:i) /= achar(13)) out_len = out_len + 1
        end do

        allocate (character(len=out_len) :: out)

        pos = 0
        do i = 1, len(text)
            if (text(i:i) /= achar(13)) then
                pos = pos + 1
                out(pos:pos) = text(i:i)
            end if
        end do
    end function strip_carriage_returns

    pure function rstrip_whitespace(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: i
        character(len=1) :: ch
        character(len=1) :: lf
        character(len=1) :: tab

        lf = new_line('A')
        tab = achar(9)

        i = len(text)
        do while (i > 0)
            ch = text(i:i)
            if (ch == ' ' .or. ch == lf .or. ch == tab) then
                i = i - 1
            else
                exit
            end if
        end do

        allocate (character(len=i) :: out)
        if (i > 0) out = text(1:i)
    end function rstrip_whitespace

end program test_docs_examples_ci_verified
