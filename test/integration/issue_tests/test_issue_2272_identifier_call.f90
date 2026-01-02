program test_issue_2272_identifier_call
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #2272: Identifier call preservation ==='

    if (.not. test_identifier_call_round_trip()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #2272 fixed!'
    else
        print *, 'Issue #2272 test failed!'
        stop 1
    end if

contains

    include '../../common/read_example.inc'

    logical function test_identifier_call_round_trip()
        character(len=:), allocatable :: source, output, error_msg

        test_identifier_call_round_trip = .true.
        print *, 'Testing identifier named call...'

        call read_example('examples/f90/issue_2272_identifier_call.f90', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: Unexpected error:', trim(error_msg)
                test_identifier_call_round_trip = .false.
                return
            end if
        end if

        if (index(output, 'integer :: call') == 0) then
            print *, '  FAIL: integer declaration missing'
            test_identifier_call_round_trip = .false.
        else
            print *, '  PASS: integer declaration preserved'
        end if

        if (index(output, 'call = 1') == 0) then
            print *, '  FAIL: assignment missing'
            test_identifier_call_round_trip = .false.
        else
            print *, '  PASS: assignment preserved'
        end if

        if (index(output, 'real :: call') > 0) then
            print *, '  FAIL: type rewritten to real'
            test_identifier_call_round_trip = .false.
        else
            print *, '  PASS: type not rewritten'
        end if

        if (index(output, '! Error: expected subroutine name after') > 0) then
            print *, '  FAIL: fabricated CALL error comment detected'
            test_identifier_call_round_trip = .false.
        else
            print *, '  PASS: no fabricated CALL error comment'
        end if
    end function test_identifier_call_round_trip


end program test_issue_2272_identifier_call
