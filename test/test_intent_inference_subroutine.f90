program test_intent_inference_subroutine
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    print *, "=== Testing Subroutine Intent Inference (Issue #2114) ==="

    call assert_intent_counts('examples/lf/issue_missing_intent_inout_subroutine.lf', &
        expected_in=0, expected_inout=1)
    call assert_intent_counts('examples/lf/intent_in_only_subroutine.lf', &
        expected_in=1, expected_inout=0)
    call assert_intent_counts('examples/lf/intent_mixed_subroutine.lf', &
        expected_in=1, expected_inout=2)

    print *, "All subroutine intent inference tests PASSED"

contains

    include 'common/read_example.inc'


    subroutine assert_intent_counts(example_path, expected_in, expected_inout)
        character(len=*), intent(in) :: example_path
        integer, intent(in) :: expected_in
        integer, intent(in) :: expected_inout
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: errors
        integer :: actual_in
        integer :: actual_inout

        call read_example(example_path, source)
        call transform_lazy_fortran_string(source, output, errors)
        call assert_no_errors(example_path, errors)

        call count_intent_markers(output, actual_in, actual_inout)

        if (actual_inout /= expected_inout .or. actual_in /= expected_in) then
            write (error_unit, '(a)') 'FAIL: intent counts mismatch for ' // &
                trim(example_path)
            write (error_unit, '(a,2i0)') '  expected (in,inout): ', expected_in, &
                expected_inout
            write (error_unit, '(a,2i0)') '  actual   (in,inout): ', actual_in, &
                actual_inout
            error stop 1
        else
            print *, 'PASS: ', trim(example_path), ' intents validated'
        end if
    end subroutine assert_intent_counts

    subroutine assert_no_errors(example_path, errors)
        character(len=*), intent(in) :: example_path
        character(len=*), intent(in) :: errors

        if (len_trim(errors) /= 0) then
            write (error_unit, '(a)') 'FAIL: transformation errors for ' // &
                trim(example_path)
            write (error_unit, '(a)') trim(errors)
            error stop 1
        end if
    end subroutine assert_no_errors

    subroutine count_intent_markers(text, in_count, inout_count)
        character(len=*), intent(in) :: text
        integer, intent(out) :: in_count
        integer, intent(out) :: inout_count
        integer :: start
        integer :: next_break
        integer :: text_len
        integer :: line_end
        character(len=:), allocatable :: line

        in_count = 0
        inout_count = 0
        text_len = len(text)
        if (text_len <= 0) return

        start = 1
        do while (start <= text_len)
            next_break = index(text(start:), new_line('a'))
            if (next_break == 0) then
                line = text(start:)
                start = text_len + 1
            else
                line_end = start + next_break - 2
                if (line_end >= start) then
                    line = text(start:line_end)
                else
                    line = ''
                end if
                start = start + next_break
            end if

            if (len(line) == 0) cycle
            if (index(line, 'intent(inout)') > 0) then
                inout_count = inout_count + 1
            else if (index(line, 'intent(in)') > 0) then
                in_count = in_count + 1
            end if
        end do
    end subroutine count_intent_markers


end program test_intent_inference_subroutine
