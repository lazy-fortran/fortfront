program test_issue_1582_simple_goto
    use fortfront, only: transform_lazy_fortran_string, tooling_parse_options_t, &
                         tooling_load_ast_from_string, ast_arena_t, token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #1582: Simple GOTO preservation ==='

    if (.not. test_simple_goto_preservation()) all_passed = .false.
    if (.not. test_goto_no_replacement()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1582 fixed!'
    else
        print *, 'Issue #1582 test failed!'
        stop 1
    end if

contains

    include '../../common/cli_io_reader.inc'
    include '../../common/read_example.inc'

    logical function test_simple_goto_preservation()
        character(len=:), allocatable :: source, output, error_msg
        type(tooling_parse_options_t) :: options

        test_simple_goto_preservation = .true.
        print *, 'Testing simple goto preservation...'

        call read_example('examples/f90/issue_1582_simple_goto.f90', source)

        options = tooling_parse_options_t()
        options%run_semantics = .false.

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error:', trim(error_msg)
                test_simple_goto_preservation = .false.
                return
            end if
        end if

        ! Check if goto statement is present in output
        if (index(output, 'go to 100') == 0 .and. index(output, 'goto 100') == 0) then
            print *, '  FAIL: goto statement missing in output'
            test_simple_goto_preservation = .false.
        else
            print *, '  PASS: goto statement present'
        end if

        ! Check for numeric label 100
        if (index(output, '100 continue') == 0 .and. &
            index(output, '100continue') == 0) then
            print *, '  FAIL: label 100 missing in output'
            test_simple_goto_preservation = .false.
        else
            print *, '  PASS: label 100 preserved'
        end if

        ! Verify correct program name
        if (index(output, 'program test_goto') == 0) then
            print *, '  FAIL: wrong program name in output'
            test_simple_goto_preservation = .false.
        else
            print *, '  PASS: correct program name'
        end if
    end function test_simple_goto_preservation

    logical function test_goto_no_replacement()
        character(len=:), allocatable :: source, output, error_msg
        type(tooling_parse_options_t) :: options

        test_goto_no_replacement = .true.
        print *, 'Testing that goto is not replaced with wrong code...'

        call read_example('examples/f90/issue_1582_simple_goto.f90', source)

        options = tooling_parse_options_t()
        options%run_semantics = .false.

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error:', trim(error_msg)
                test_goto_no_replacement = .false.
                return
            end if
        end if

        ! Ensure output does NOT contain wrong program name demo
        if (index(output, 'program demo') > 0) then
            print *, '  FAIL: program replaced with wrong name demo'
            test_goto_no_replacement = .false.
        else
            print *, '  PASS: program name not replaced'
        end if

        ! Ensure output does NOT contain unrelated loop code
        if (index(output, '10 i = i + 1') > 0) then
            print *, '  FAIL: goto replaced with unrelated loop code'
            test_goto_no_replacement = .false.
        else
            print *, '  PASS: no unrelated loop code injected'
        end if

        ! Check that i = 999 assignment is present
        if (index(output, 'i = 999') == 0) then
            print *, '  FAIL: unreachable code i = 999 was removed'
            test_goto_no_replacement = .false.
        else
            print *, '  PASS: unreachable code preserved'
        end if
    end function test_goto_no_replacement


end program test_issue_1582_simple_goto
