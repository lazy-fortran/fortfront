program test_print_fix
    use iso_fortran_env, only: error_unit
    implicit none
    
    character(len=:), allocatable :: source
    integer :: output_unit, input_unit
    character(len=1024) :: line
    logical :: found_print
    integer :: io_status, exit_status
    
    print *, "Testing Issue #600: Print statements missing from output"
    
    ! Create test source file
    open(newunit=input_unit, file='test_print.f90', status='replace')
    write(input_unit, '(A)') 'program test'
    write(input_unit, '(A)') '    print *, "Hello, World!"'
    write(input_unit, '(A)') 'end program test'
    close(input_unit)
    
    ! Run fortfront on it
    call execute_command_line('./build/gfortran_*/app/fortfront test_print.f90 > test_print_out.f90 2>&1', exitstat=exit_status)
    
    if (exit_status /= 0) then
        write(error_unit, *) "FAIL: fortfront execution failed with status:", exit_status
        stop 1
    end if
    
    ! Check output for print statement
    found_print = .false.
    open(newunit=output_unit, file='test_print_out.f90', status='old', action='read', iostat=io_status)
    if (io_status /= 0) then
        write(error_unit, *) "FAIL: Could not open output file"
        stop 1
    end if
    
    do
        read(output_unit, '(A)', iostat=io_status) line
        if (io_status /= 0) exit
        
        if (index(line, 'print') > 0) then
            found_print = .true.
            print *, "Found print statement: ", trim(line)
        end if
    end do
    close(output_unit)
    
    if (.not. found_print) then
        write(error_unit, *) "FAIL: Print statement not found in output"
        write(error_unit, *) "Output file contents:"
        open(newunit=output_unit, file='test_print_out.f90', status='old')
        do
            read(output_unit, '(A)', iostat=io_status) line
            if (io_status /= 0) exit
            write(error_unit, '(A)') trim(line)
        end do
        close(output_unit)
        stop 1
    end if
    
    print *, "SUCCESS: Print statement preserved in output"
    
    ! Cleanup
    call execute_command_line('rm -f test_print.f90 test_print_out.f90')
    
end program test_print_fix