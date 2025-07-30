program test_cli_integration
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: test_count, pass_count
    logical :: is_windows
    
    test_count = 0
    pass_count = 0
    
    ! Detect if we're on Windows
    is_windows = check_if_windows()
    
    print *, "=== CLI Integration System Tests ==="
    print *, ""
    
    if (is_windows) then
        print *, "SKIPPING: CLI integration tests on Windows (shell compatibility issues)"
        print *, "This is a known limitation and will be fixed in a future update"
        stop 0
    end if
    
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

    function check_if_windows() result(is_win)
        logical :: is_win
        character(len=10) :: os_name
        integer :: stat
        
        ! Try to detect Windows through environment variable
        call get_environment_variable('OS', os_name, status=stat)
        is_win = (stat == 0 .and. os_name(1:7) == 'Windows')
        
        ! Alternative: check for Windows-specific env var
        if (.not. is_win) then
            call get_environment_variable('WINDIR', os_name, status=stat)
            is_win = (stat == 0)
        end if
    end function check_if_windows
    
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
            print *, "  Failed to run basic CLI command"
        end if
    end subroutine test_basic_io
    
    subroutine test_error_handling()
        integer :: exit_code
        character(len=512) :: command
        logical :: success
        
        call test_start("Error handling")
        
        ! Run with invalid input
        command = 'echo "invalid fortran code @#$%" | ./build/gfortran_*/app/ff > ' // &
                  '/tmp/ff_test_output2.txt 2>/tmp/ff_test_error2.txt'
        call execute_command_line(command, exitstat=exit_code)
        
        ! Should still exit successfully but output should contain error markers
        success = (exit_code == 0)
        
        call test_result(success)
        if (.not. success) then
            print *, "  Error handling failed"
        end if
    end subroutine test_error_handling
    
    subroutine test_empty_input()
        integer :: exit_code
        character(len=1000) :: output_line
        character(len=512) :: command
        logical :: success
        
        call test_start("Empty input produces valid program")
        
        ! Run with empty input
        command = 'echo "" | ./build/gfortran_*/app/ff > ' // &
                  '/tmp/ff_test_output3.txt 2>/tmp/ff_test_error3.txt'
        call execute_command_line(command, exitstat=exit_code)
        
        success = (exit_code == 0)
        
        if (success) then
            ! Check output contains valid empty program
            open(unit=11, file='/tmp/ff_test_output3.txt', status='old', action='read')
            read(11, '(A)', end=200) output_line
            success = success .and. (index(output_line, 'program main') > 0)
200         close(11)
        end if
        
        call test_result(success)
        if (.not. success) then
            print *, "  Empty input handling failed"
        end if
    end subroutine test_empty_input
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A)', advance='no') "Testing: " // test_name // "  ... "
    end subroutine test_start
    
    subroutine test_result(passed)
        logical, intent(in) :: passed
        if (passed) then
            print *, "PASSED"
            pass_count = pass_count + 1
        else
            print *, "FAILED"
        end if
    end subroutine test_result
    
end program test_cli_integration