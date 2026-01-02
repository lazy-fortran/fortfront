program test_cli_io_allocation_failure
    implicit none

    character(len=:), allocatable :: text
    integer :: status

    ! Test 1: Normal small input should work (status = 0)
    call test_small_input()

    ! Test 2: Deterministically cover allocation-failure branch (status = 5)
    call test_forced_allocation_failure()

    print *, 'PASS: CLI I/O allocation failure branch covered'

contains

    subroutine test_small_input()
        character(len=*), parameter :: fname = 'tmp_small_test.txt'
        integer :: u

        ! Create a small test file
        open (newunit=u, file=fname, status='replace', action='write')
        write (u, '(A)') 'x = 42'
        close (u)

        call read_all_stdin_or_file(.true., fname, text, status)

        if (status /= 0) then
            print *, 'FAIL: Small input failed with status', status
            stop 1
        end if

        if (.not. allocated(text)) then
            print *, 'FAIL: Text not allocated for small input'
            stop 1
        end if

        ! Cleanup
        open (newunit=u, file=fname, status='old')
        close (u, status='delete')

        print *, 'INFO: Small input test passed (status=0)'
    end subroutine test_small_input

    subroutine test_forced_allocation_failure()
        character(len=*), parameter :: fname = 'tmp_alloc_fail_test.txt'
        integer :: u

        open (newunit=u, file=fname, status='replace', action='write')
        write (u, '(A)') 'x = 42'
        close (u)

        call read_all_stdin_or_file(.true., fname, text, status, &
                                    test_force_alloc_failure=.true.)

        if (status /= 5) then
            print *, 'FAIL: Expected status=5 for allocation failure, got', status
            stop 1
        end if

        if (allocated(text)) then
            print *, 'FAIL: Text should not be allocated on allocation failure'
            stop 1
        end if

        open (newunit=u, file=fname, status='old')
        close (u, status='delete')

        print *, 'INFO: Forced allocation failure returned status=5'
    end subroutine test_forced_allocation_failure

    include '../common/read_example.inc'

end program test_cli_io_allocation_failure
