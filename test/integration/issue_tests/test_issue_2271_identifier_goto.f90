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

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: unit, file_size, stat
        character(len=1), allocatable :: buffer(:)

        open (newunit=unit, file=filepath, status='old', access='stream', &
              form='unformatted', iostat=stat)
        if (stat /= 0) error stop 'Failed to open example file: ' // filepath

        inquire (unit=unit, size=file_size)
        allocate (buffer(file_size))
        read (unit, iostat=stat) buffer
        if (stat /= 0) error stop 'Failed to read example file: ' // filepath
        close (unit)

        allocate (character(len=file_size) :: content)
        content = transfer(buffer, content)
    end subroutine read_example

end program test_issue_2271_identifier_goto
