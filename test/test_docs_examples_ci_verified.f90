program test_docs_examples_ci_verified
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

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

    logical function test_character_length_inference()
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_character_length_inference = .true.

        call read_example('examples/lf/docs_character_length_inference.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        call assert_transform_ok('docs_character_length_inference', output, &
                                 error_msg, test_character_length_inference)
        if (.not. test_character_length_inference) return

        call assert_contains('docs_character_length_inference', output, &
                             'character(len=5) :: name', &
                             test_character_length_inference)
        call assert_contains('docs_character_length_inference', output, &
                             'name = "hello"', test_character_length_inference)
    end function test_character_length_inference

    logical function test_string_concatenation()
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_string_concatenation = .true.

        call read_example('examples/lf/docs_string_concatenation.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        call assert_transform_ok('docs_string_concatenation', output, error_msg, &
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
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_variable_length_strings = .true.

        call read_example('examples/lf/docs_variable_length_strings.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        call assert_transform_ok('docs_variable_length_strings', output, error_msg, &
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
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_fixed_length_reassignment = .true.

        call read_example('examples/lf/docs_fixed_length_reassignment.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        call assert_transform_ok('docs_fixed_length_reassignment', output, &
                                 error_msg, test_fixed_length_reassignment)
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
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_character_arrays = .true.

        call read_example('examples/lf/docs_character_arrays.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        call assert_transform_ok('docs_character_arrays', output, error_msg, &
                                 test_character_arrays)
        if (.not. test_character_arrays) return

        call assert_contains('docs_character_arrays', output, &
                             'character(len=7) :: names(3)', test_character_arrays)
        call assert_contains('docs_character_arrays', output, &
                             'names = ["alice  ", "bob    ", "charlie"]', &
                             test_character_arrays)
    end function test_character_arrays

    logical function test_type_validation_calculate()
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_type_validation_calculate = .true.

        call read_example('examples/lf/docs_type_validation_calculate.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        call assert_transform_ok('docs_type_validation_calculate', output, &
                                 error_msg, test_type_validation_calculate)
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
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_mixed_type_operations = .true.

        call read_example('examples/lf/docs_mixed_type_operations.lf', input)
        call transform_lazy_fortran_string(input, output, error_msg)

        call assert_transform_ok('docs_mixed_type_operations', output, error_msg, &
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

end program test_docs_examples_ci_verified
