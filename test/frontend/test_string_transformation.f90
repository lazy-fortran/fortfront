program test_string_transformation
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    if (.not. run_example_test( &
        'hello world', &
        'examples/lf/string_transform_hello.lf', &
        [character(len=32) :: 'program main', 'implicit none', &
        'print *, ''Hello'''])) then
        all_passed = .false.
    end if

    if (.not. run_example_test( &
        'control flow', &
        'examples/lf/string_transform_control_flow.lf', &
        [character(len=32) :: 'if (x > 0) then', 'end if'])) then
        all_passed = .false.
    end if

    if (.not. run_example_test( &
        'multiple statements', &
        'examples/lf/string_transform_multiple_statements.lf', &
        [character(len=48) :: 'integer :: a, b, c', 'print *, c'])) then
        all_passed = .false.
    end if

    if (.not. run_example_test( &
        'string concatenation', &
        'examples/lf/string_transform_concat.lf', &
        [character(len=32) :: '//', 'print *, t'])) then
        all_passed = .false.
    end if

    if (.not. run_example_test( &
        'complex expression', &
        'examples/lf/string_transform_complex_expression.lf', &
        [character(len=48) :: 'result = (x * 2 + y) / 3.0', 'integer :: x', &
        'real(dp) :: result, y'])) then
        all_passed = .false.
    end if

    if (.not. run_example_test( &
        'non-character declarations', &
        'examples/lf/string_transform_non_character.lf', &
        [character(len=40) :: 'integer :: n', 's = s // ''y'''])) then
        all_passed = .false.
    end if

    if (.not. test_error_handling()) all_passed = .false.
    if (.not. test_empty_input()) all_passed = .false.

    if (all_passed) then
        print *, 'PASS: string transformation regression suite'
        stop 0
    else
        error stop 'FAIL: string transformation regression suite'
    end if

contains

    include '../common/read_example.inc'


    logical function run_example_test(name, example_path, patterns) result(ok)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: example_path
        character(len=*), dimension(:), intent(in) :: patterns
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        integer :: i

        ok = .true.
        call read_example(example_path, source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A,A)') 'FAIL: ', trim(name) // ' produced error'
            write (error_unit, '(A)') trim(error_msg)
            ok = .false.
            return
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A,A)') 'FAIL: ', trim(name) // ' produced no output'
            ok = .false.
            return
        end if

        do i = 1, size(patterns)
            if (len_trim(patterns(i)) == 0) cycle
            if (.not. contains_pattern(output, trim(patterns(i)))) then
                write (error_unit, '(A,A)') 'FAIL: pattern missing for ', trim(name)
                write (error_unit, '(A)') 'Pattern: ' // trim(patterns(i))
                write (error_unit, '(A)') trim(output)
                ok = .false.
            end if
        end do
    end function run_example_test

    logical function test_error_handling()
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_error_handling = .true.
        call transform_lazy_fortran_string('invalid fortran !!!', output, &
            error_msg)
        if (.not. allocated(output) .and. len_trim(error_msg) == 0) then
            write (error_unit, '(A)') 'FAIL: error handling produced no feedback'
            test_error_handling = .false.
        end if
    end function test_error_handling

    logical function test_empty_input()
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        test_empty_input = .true.
        call transform_lazy_fortran_string('', output, error_msg)

        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: empty input produced error: ' // &
                trim(error_msg)
            test_empty_input = .false.
            return
        end if

        if (.not. allocated(output) .or. index(output, 'program main') == 0) then
            write (error_unit, '(A)') 'FAIL: empty input did not produce program'
            test_empty_input = .false.
        end if
    end function test_empty_input

    logical function contains_pattern(buffer, pattern)
        character(len=*), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: compact_buffer
        character(len=:), allocatable :: compact_pattern

        contains_pattern = index(buffer, pattern) > 0
        if (contains_pattern) return

        compact_buffer = remove_spaces(buffer)
        compact_pattern = remove_spaces(pattern)
        contains_pattern = index(compact_buffer, compact_pattern) > 0
    end function contains_pattern

    pure function remove_spaces(value) result(compacted)
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: compacted
        integer :: i

        compacted = ''
        do i = 1, len_trim(value)
            if (value(i:i) /= ' ' .and. value(i:i) /= new_line('a')) then
                compacted = compacted // value(i:i)
            end if
        end do
    end function remove_spaces

end program test_string_transformation
