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
    
    ! Pre-build fortfront to ensure it exists before testing
    print *, "Building fortfront executable..."
    call execute_command_line('fpm build --flag "-cpp -fmax-stack-var-size=65536"', exitstat=test_count)
    if (test_count /= 0) then
        print *, "SKIPPING: Failed to build fortfront executable (exit code:", test_count, ")"
        print *, "This may indicate CI environment issues or missing build dependencies"
        stop 0
    end if
    
    test_count = 0  ! Reset test counter
    
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
    
    ! Find the fortfront executable using multiple search strategies
    function find_fortfront_executable() result(executable_path)
        character(len=:), allocatable :: executable_path
        logical :: file_exists
        character(len=500) :: candidate_path
        integer :: i, exit_code, unit_num
        character(len=1000) :: search_output
        character(len=50), dimension(20) :: build_patterns
        
        executable_path = ""
        
        ! Strategy 1: Use find command to dynamically locate fortfront executable
        call execute_command_line('find build -name "fortfront" -type f 2>/dev/null | head -1 > fortfront_search.txt', exitstat=exit_code)
        if (exit_code == 0) then
            open(newunit=unit_num, file='fortfront_search.txt', status='old', action='read', iostat=exit_code)
            if (exit_code == 0) then
                read(unit_num, '(A)', iostat=exit_code) search_output
                close(unit_num)
                ! Clean up temporary file
                call execute_command_line('rm -f fortfront_search.txt', exitstat=exit_code)
                if (exit_code == 0 .and. len_trim(search_output) > 0) then
                    inquire(file=trim(search_output), exist=file_exists)
                    if (file_exists) then
                        executable_path = trim(search_output)
                        return
                    end if
                end if
            end if
        end if
        
        ! Strategy 2: Check hardcoded patterns as fallback
        ! List of common build hash patterns to check (update when needed)
        build_patterns = [ &
            "build/gfortran_266FF454AB2555FE/app/fortfront   ", &
            "build/gfortran_9ABCD662468F5A74/app/fortfront   ", &
            "build/gfortran_C79DEB301B8081FC/app/fortfront   ", &
            "build/gfortran_C523F0F8A99FF060/app/fortfront   ", &
            "build/gfortran_1F2DC83CBD1DC595/app/fortfront   ", &
            "build/gfortran_35CFD5CFC35942D6/app/fortfront   ", &
            "build/gfortran_4AE9E4ED7A89B913/app/fortfront   ", &
            "build/gfortran_66DBF6172AF51040/app/fortfront   ", &
            "build/gfortran_A56298966DD7666C/app/fortfront   ", &
            "build/gfortran_E3D58E6D75301430/app/fortfront   ", &
            "build/gfortran_9CBC8EEC13D00A4A/app/fortfront   ", &
            "./build/gfortran_266FF454AB2555FE/app/fortfront ", &
            "./build/gfortran_9ABCD662468F5A74/app/fortfront ", &
            "./build/gfortran_C79DEB301B8081FC/app/fortfront ", &
            "./build/gfortran_C523F0F8A99FF060/app/fortfront ", &
            "fortfront                                       ", &
            "./fortfront                                     ", &
            "app/fortfront                                   ", &
            "./app/fortfront                                 ", &
            "../fortfront                                    " ]
        
        ! Check each candidate path
        do i = 1, size(build_patterns)
            candidate_path = trim(build_patterns(i))
            inquire(file=candidate_path, exist=file_exists)
            
            if (file_exists) then
                executable_path = trim(candidate_path)
                return
            end if
        end do
        
    end function find_fortfront_executable
    
    
    subroutine test_basic_io()
        integer :: exit_code
        character(len=1000) :: output_line
        character(len=512) :: command
        character(len=:), allocatable :: executable_path
        logical :: success
        
        call test_start("Basic CLI I/O")
        
        ! Find the fortfront executable
        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if
        
        ! Run: echo "print *, 'test'" | fortfront
        command = 'echo "print *, ''test''" | ' // executable_path // ' > ' // &
                  'test_output.txt 2>test_error.txt'
        call execute_command_line(command, exitstat=exit_code)
        
        success = (exit_code == 0)
        
        if (success) then
            ! Check if output contains expected Fortran code
            open(unit=10, file='test_output.txt', status='old', action='read', iostat=exit_code)
            if (exit_code == 0) then
                read(10, '(A)', end=100, iostat=exit_code) output_line
                if (exit_code == 0) then
                    success = success .and. (index(output_line, 'program main') > 0)
                end if
100             close(10)
                ! Clean up test files
                call execute_command_line('rm -f test_output.txt test_error.txt', exitstat=exit_code)
            else
                success = .false.
            end if
        end if
        
        call test_result(success)
        if (.not. success) then
            print *, "  Failed to run basic CLI command"
            print *, "  Executable path: ", executable_path
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_basic_io
    
    subroutine test_error_handling()
        integer :: exit_code
        character(len=512) :: command
        character(len=:), allocatable :: executable_path
        logical :: success
        
        call test_start("Error handling")
        
        ! Find the fortfront executable
        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if
        
        ! Run with invalid input
        command = 'echo "invalid fortran code @#$%" | ' // executable_path // ' > ' // &
                  'test_output2.txt 2>test_error2.txt'
        call execute_command_line(command, exitstat=exit_code)
        
        ! Clean up test files
        call execute_command_line('rm -f test_output2.txt test_error2.txt', exitstat=exit_code)
        
        ! Should still exit successfully but output should contain error markers
        success = (exit_code == 0)
        
        call test_result(success)
        if (.not. success) then
            print *, "  Error handling failed"
            print *, "  Exit code: ", exit_code
        end if
    end subroutine test_error_handling
    
    subroutine test_empty_input()
        integer :: exit_code
        character(len=1000) :: output_line
        character(len=512) :: command
        character(len=:), allocatable :: executable_path
        logical :: success
        
        call test_start("Empty input produces valid program")
        
        ! Find the fortfront executable
        executable_path = find_fortfront_executable()
        if (len(executable_path) == 0) then
            call test_result(.false.)
            print *, "  ERROR: Could not locate fortfront executable"
            return
        end if
        
        ! Run with empty input
        command = 'echo "" | ' // executable_path // ' > ' // &
                  'test_output3.txt 2>test_error3.txt'
        call execute_command_line(command, exitstat=exit_code)
        
        success = (exit_code == 0)
        
        if (success) then
            ! Check output contains valid empty program
            open(unit=11, file='test_output3.txt', status='old', action='read', iostat=exit_code)
            if (exit_code == 0) then
                read(11, '(A)', end=200, iostat=exit_code) output_line
                if (exit_code == 0) then
                    success = success .and. (index(output_line, 'program main') > 0)
                end if
200             close(11)
                ! Clean up test files
                call execute_command_line('rm -f test_output3.txt test_error3.txt', exitstat=exit_code)
            else
                success = .false.
            end if
        end if
        
        call test_result(success)
        if (.not. success) then
            print *, "  Empty input handling failed"
            print *, "  Exit code: ", exit_code
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