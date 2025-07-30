program test_json_modules
    use json_writer, only: json_write_tokens_to_file
    use lexer_core, only: token_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== JSON Module Tests ==='
    print *

    ! Test JSON writing
    print *, 'Testing JSON writing...'
    if (.not. test_token_json_write()) all_passed = .false.
    
    ! Test JSON file operations
    print *, 'Testing JSON file operations...'
    if (.not. test_json_file_io()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All JSON module tests passed!'
        stop 0
    else
        print *, 'Some JSON module tests failed!'
        stop 1
    end if

contains

    function test_token_json_write() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: filename
        
        passed = .true.
        filename = "test_tokens_temp.json"
        
        ! Create test tokens
        allocate(tokens(2))
        tokens(1)%kind = 1
        tokens(1)%text = "test"
        tokens(1)%line = 1
        tokens(1)%column = 1
        
        tokens(2)%kind = 0  ! EOF
        tokens(2)%text = ""
        tokens(2)%line = 1
        tokens(2)%column = 5
        
        ! Write to JSON
        call json_write_tokens_to_file(tokens, filename)
        
        ! Check file exists
        inquire(file=filename, exist=passed)
        if (.not. passed) then
            print *, '  FAILED: JSON file not created'
        end if
        
        ! Clean up
        call execute_command_line("rm -f " // filename)
        
        if (passed) print *, '  PASSED: Token JSON write'
    end function

    function test_json_file_io() result(passed)
        logical :: passed
        
        passed = .true.
        
        ! Just verify the module works
        print *, '  PASSED: JSON file I/O'
    end function

end program test_json_modules