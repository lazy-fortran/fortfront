program test_issue_2077_goto_labeled_statements
    use fortfront, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #2077: GOTO labeled statements preservation ==='

    if (.not. test_goto_labeled_statements()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #2077 fixed!'
    else
        print *, 'Issue #2077 test failed!'
        stop 1
    end if

contains

    include '../../common/cli_io_reader.inc'
    include '../../common/read_example.inc'


    logical function test_goto_labeled_statements()
        character(len=:), allocatable :: source, output, error_msg

        test_goto_labeled_statements = .true.
        print *, 'Testing goto labeled statements preservation...'

        call read_example('examples/lf/issue_2077_goto_drops_labeled_statements.lf', source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error:', trim(error_msg)
                test_goto_labeled_statements = .false.
                return
            end if
        end if

        ! Check if goto statements are present in output
        if (index(output, 'go to 100') == 0 .and. index(output, 'goto 100') == 0) then
            print *, '  FAIL: goto 100 statement missing in output'
            test_goto_labeled_statements = .false.
        else
            print *, '  PASS: goto 100 statement present'
        end if

        if (index(output, 'go to 200') == 0 .and. index(output, 'goto 200') == 0) then
            print *, '  FAIL: goto 200 statement missing in output'
            test_goto_labeled_statements = .false.
        else
            print *, '  PASS: goto 200 statement present'
        end if

        ! Check for labeled print statement (label 100)
        if (index(output, '100 ') == 0) then
            print *, '  FAIL: label 100 missing in output'
            test_goto_labeled_statements = .false.
        else
            print *, '  PASS: label 100 preserved'
        end if

        ! Check that labeled statement at 100 has content (print statement)
        if (index(output, 'print') > 0 .and. index(output, 'Large') > 0) then
            print *, '  PASS: labeled statement at 100 has print content'
        else
            print *, '  FAIL: labeled statement at 100 missing print content'
            test_goto_labeled_statements = .false.
        end if

        ! Check for labeled continue statement (label 200)
        if (index(output, '200 ') == 0) then
            print *, '  FAIL: label 200 missing in output'
            test_goto_labeled_statements = .false.
        else
            print *, '  PASS: label 200 preserved'
        end if

        ! Check that labeled statement at 200 has content (assignment)
        if (index(output, 'x = x + 1') > 0 .or. index(output, 'x=x+1') > 0) then
            print *, '  PASS: labeled statement at 200 has assignment'
        else
            print *, '  FAIL: labeled statement at 200 missing assignment'
            test_goto_labeled_statements = .false.
        end if

        ! Verify no bare labels (labels without statements)
        ! This is the core bug - labels should always have statements
        ! Check that labels are followed by actual statements, not just newlines
        if ((index(output, '100 print') > 0 .or. index(output, '100 PRINT') > 0) .and. &
            (index(output, '200 x') > 0 .or. index(output, '200 X') > 0)) then
            print *, '  PASS: All labels have statements'
        else
            print *, '  FAIL: Found bare label without statement'
            test_goto_labeled_statements = .false.
        end if
    end function test_goto_labeled_statements

end program test_issue_2077_goto_labeled_statements
