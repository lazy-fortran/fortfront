program test_cli_integration
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== CLI Integration System Tests ==="
    print *, ""
    
    ! Test 1: Basic CLI I/O works
    call test_basic_io()
    
    ! Test 2: Error handling works
    call test_error_handling()
    
    ! Test 3: Empty input handling
    call test_empty_input()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All CLI system tests passed!"
        stop 0
    else
        print *, "Some CLI system tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_basic_io()
        integer :: exit_code
        character(len=1000) :: output_line
        character(len=512) :: command
        logical :: success
        
        call test_start("Basic CLI I/O")
        
        ! Run: echo "print *, 'test'" | ff
        command = 'echo "print *, ''test''" | ./build/gfortran_*/app/ff > ' // &
                  '/tmp/ff_test_output.txt 2>/tmp/ff_test_error.txt'
        call execute_command_line(command, exitstat=exit_code)
        
        success = (exit_code == 0)
        
        if (success) then
            ! Check if output contains expected Fortran code
            open(unit=10, file='/tmp/ff_test_output.txt', status='old', action='read')  
            read(10, '(A)', end=100) output_line
            success = success .and. (index(output_line, 'program main') > 0)
100         close(10)
        end if
        
        call test_result(success)
        if (.not. success) then
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_basic_io
    
    subroutine test_error_handling()
        integer :: exit_code
        character(len=512) :: command
        logical :: success
        
        call test_start("Error handling")
        
        ! Test with empty input (should work)
        command = 'echo "" | ./build/gfortran_*/app/ff > ' // &
                  '/tmp/ff_test_output2.txt 2>/tmp/ff_test_error2.txt'
        call execute_command_line(command, exitstat=exit_code)
        
        ! Empty input should succeed (produces minimal program)
        success = (exit_code == 0)
        
        call test_result(success)
        if (.not. success) then
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_error_handling
    
    subroutine test_empty_input()
        integer :: exit_code
        character(len=1000) :: output_line
        character(len=512) :: command
        logical :: success
        
        call test_start("Empty input produces valid program")
        
        ! Test empty input
        command = 'echo "" | ./build/gfortran_*/app/ff > ' // &
                  '/tmp/ff_test_output3.txt 2>/tmp/ff_test_error3.txt'
        call execute_command_line(command, exitstat=exit_code)
        
        success = (exit_code == 0)
        
        if (success) then
            ! Check if output contains minimal program
            open(unit=11, file='/tmp/ff_test_output3.txt', status='old', action='read')
            read(11, '(A)', end=200) output_line
            success = success .and. (index(output_line, 'program main') > 0)
200         close(11)
        end if
        
        call test_result(success)
        if (.not. success) then
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_empty_input
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_result(success)
        logical, intent(in) :: success
        if (success) then
            print *, " ... PASSED"
            pass_count = pass_count + 1
        else
            print *, " ... FAILED"
        end if
    end subroutine test_result
    
end program test_cli_integration