program test_undeclared_variable_error
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: passed
    character(len=:), allocatable :: command, test_file, exe_file
    integer :: exit_code, unit

    print *, "=== Undeclared Variable Error Test ==="
    print *

    passed = .false.

    ! Create test program with undeclared variable
    test_file = "test_undeclared.f90"
    exe_file = "test_undeclared"

    open (newunit=unit, file=test_file, status='replace')
    write (unit, '(a)') 'program test'
    write (unit, '(a)') '    undeclared_var = 42'  ! This should cause error
    write (unit, '(a)') '    print *, undeclared_var'
    write (unit, '(a)') 'end program test'
    close (unit)

    ! Try to compile - this SHOULD fail
    command = 'fortran --compile -o '//exe_file//' '//test_file
    call execute_command_line(command, exitstat=exit_code)

    if (exit_code /= 0) then
        print *, "PASS: Undeclared variable correctly rejected"
        passed = .true.
    else
        print *, "FAIL: Undeclared variable was accepted (should be rejected)"
        print *, "  Compilation should have failed with undeclared variable error"
    end if

    ! Cleanup
    ! Clean up files - ignore if they don't exist
    open(newunit=unit, file=test_file, status='old', iostat=exit_code)
    if (exit_code == 0) close(unit, status='delete')
    open(newunit=unit, file=exe_file, status='old', iostat=exit_code)
    if (exit_code == 0) close(unit, status='delete')

    print *
    if (passed) then
        print *, "Test passed!"
        stop 0
    else
        print *, "Test failed!"
        stop 1
    end if

end program test_undeclared_variable_error
