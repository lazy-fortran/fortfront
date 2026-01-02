program test_issue_2271_identifier_goto
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #2271: Identifier goto preservation ==='

    if (.not. test_identifier_goto_round_trip()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #2271 fixed!'
    else
        print *, 'Issue #2271 test failed!'
        stop 1
    end if

contains

    include '../../common/read_example.inc'

    logical function test_identifier_goto_round_trip()
        character(len=:), allocatable :: source, output, error_msg

        test_identifier_goto_round_trip = .true.
        print *, 'Testing identifier named goto...'

        call read_example('examples/f90/issue_2271_identifier_goto.f90', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error:', trim(error_msg)
                test_identifier_goto_round_trip = .false.
                return
            end if
        end if

        if (index(output, 'integer :: goto') == 0) then
            print *, '  FAIL: integer declaration missing'
            test_identifier_goto_round_trip = .false.
        else
            print *, '  PASS: integer declaration preserved'
        end if

        if (index(output, 'goto = 1') == 0) then
            print *, '  FAIL: assignment to goto missing'
            test_identifier_goto_round_trip = .false.
        else
            print *, '  PASS: assignment preserved'
        end if

        if (index(output, 'real :: goto') > 0) then
            print *, '  FAIL: type rewritten to real'
            test_identifier_goto_round_trip = .false.
        else
            print *, '  PASS: type not rewritten'
        end if

        if (index(output, 'go to ') > 0) then
            print *, '  FAIL: fabricated GO TO statement detected'
            test_identifier_goto_round_trip = .false.
        else
            print *, '  PASS: no GO TO injected'
        end if
    end function test_identifier_goto_round_trip


end program test_issue_2271_identifier_goto
