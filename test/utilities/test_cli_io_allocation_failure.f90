program test_cli_io_allocation_failure
    use, intrinsic :: iso_fortran_env, only: error_unit
    use test_filesystem_helpers, only: check_if_windows, cleanup_file, &
        make_temp_file_path
    implicit none

    character(len=:), allocatable :: text
    integer :: status
    logical :: is_windows

    is_windows = check_if_windows()

    ! Test 1: Normal small input should work (status = 0)
    call test_small_input(is_windows)

    ! Test 2: Deterministically cover allocation-failure branch (status = 5)
    call test_forced_allocation_failure(is_windows)

    print *, 'PASS: CLI I/O allocation failure branch covered'

contains

    subroutine test_small_input(is_windows)
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: fname
        integer :: u

        fname = make_temp_file_path('ff_cli_io_small_', '.txt', is_windows)

        ! Create a small test file
        open (newunit=u, file=fname, status='replace', action='write')
        write (u, '(A)') 'x = 42'
        close (u)

        call read_all_stdin_or_file(.true., fname, text, status)

        if (status /= 0) then
            write (error_unit, '(A,I0)') &
                'FAIL: Small input failed with status ', status
            stop 1
        end if

        if (.not. allocated(text)) then
            write (error_unit, '(A)') 'FAIL: Text not allocated for small input'
            stop 1
        end if

        ! Cleanup
        call cleanup_file(fname)

        print *, 'INFO: Small input test passed (status=0)'
    end subroutine test_small_input

    subroutine test_forced_allocation_failure(is_windows)
        logical, intent(in) :: is_windows
        character(len=:), allocatable :: fname
        integer :: u

        fname = make_temp_file_path('ff_cli_io_alloc_fail_', '.txt', is_windows)

        open (newunit=u, file=fname, status='replace', action='write')
        write (u, '(A)') 'x = 42'
        close (u)

        call read_all_stdin_or_file(.true., fname, text, status, &
            test_force_alloc_failure=.true.)

        if (status /= 5) then
            write (error_unit, '(A,I0)') &
                'FAIL: Expected status=5 for allocation failure, got ', status
            stop 1
        end if

        if (allocated(text)) then
            write (error_unit, '(A)') &
                'FAIL: Text should not be allocated on allocation failure'
            stop 1
        end if

        call cleanup_file(fname)

        print *, 'INFO: Forced allocation failure returned status=5'
    end subroutine test_forced_allocation_failure

    include '../common/read_example.inc'

end program test_cli_io_allocation_failure
