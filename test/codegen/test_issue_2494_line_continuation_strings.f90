program test_issue_2494_line_continuation_strings
    use, intrinsic :: iso_fortran_env, only: error_unit
    use codegen_basic_utils, only: add_line_continuations
    implicit none

    character(len=:), allocatable :: input_line, output

    ! Test: Ensure line continuation does not break inside string literals
    ! Per ISO/IEC 1539-1:2018 Section 6.3.2.5, breaking inside strings
    ! requires special continuation syntax. This test verifies we avoid
    ! breaking inside string content entirely.
    input_line = 'print "(a, i8)", "Line 1", array(2), "Line 3", ' // &
        'array(4), "Line 5", array(6), "Line 7", array(8), ' // &
        '"Line 9", array(10), "Line 11", array(12), "Line 13"'
    output = add_line_continuations(input_line)

    ! The original bug caused Line 11 to be broken as Line  &\n11
    ! where the continuation marker appeared inside the string literal.
    ! This corrupted the string content by adding extra spaces.

    ! Check for the specific bug symptom: string literal broken with & inside
    if (index(output, '"Line  &') > 0) then
        write (error_unit, '(A)') 'FAIL: String "Line 11" broken with & inside'
        write (error_unit, '(A)') 'Output was:'
        write (error_unit, '(A)') output
        stop 1
    end if

    ! Verify the full string Line 11 is present unchanged
    if (index(output, '"Line 11"') == 0) then
        write (error_unit, '(A)') 'FAIL: String "Line 11" not found intact'
        write (error_unit, '(A)') 'Output was:'
        write (error_unit, '(A)') output
        stop 1
    end if

    ! Verify continuation does happen (line is long enough to need it)
    if (index(output, ' &') == 0) then
        write (error_unit, '(A)') 'FAIL: Expected line continuation for long line'
        stop 1
    end if

    ! Additional check: doubled quotes inside a string literal stay intact
    input_line = 'print "(a, i8)", "Entry ""A"" text", array(2), ' // &
        '"Entry ""B"" text", array(4), "Entry ""C"" text", ' // &
        'array(6), "Entry ""D"" text", array(8), ' // &
        '"Entry ""E"" text", array(10)'
    output = add_line_continuations(input_line)

    if (index(output, '"Entry ""A""  &') > 0) then
        write (error_unit, '(A)') 'FAIL: Doubled quotes broken by continuation'
        write (error_unit, '(A)') 'Output was:'
        write (error_unit, '(A)') output
        stop 1
    end if

    if (index(output, '"Entry ""A"" text"') == 0) then
        write (error_unit, '(A)') 'FAIL: Doubled quote string missing after continue'
        write (error_unit, '(A)') 'Output was:'
        write (error_unit, '(A)') output
        stop 1
    end if

    if (index(output, ' &') == 0) then
        write (error_unit, '(A)') 'FAIL: Expected continuation for doubled quote case'
        stop 1
    end if

    print *, 'PASS: Line continuation preserves string literal contents'

end program test_issue_2494_line_continuation_strings
