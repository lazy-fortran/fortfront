program test_external_tool_integration
    ! RED Phase Test: External Tool Integration Verification
    ! Issue #416: Foundation: Create libfortfront.a static library  
    !
    ! Given: An external Fortran program that needs to use fortfront functionality
    ! When: The program is compiled and linked with libfortfront.a
    ! Then: The external tool should successfully access fortfront via pure Fortran interfaces
    !
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== External Tool Integration Tests ==="
    print *, ""
    
    ! Test 1: Pure Fortran tool compilation
    call test_pure_fortran_tool_compilation()
    
    ! Test 2: Static linking verification
    call test_static_linking_works()
    
    ! Test 3: Module usage from external program
    call test_module_usage_from_external()
    
    ! Test 4: Self-contained executable creation
    call test_self_contained_executable()
    
    ! Test 5: Multiple tool integration
    call test_multiple_tool_integration()
    
    print *, ""
    print *, "=== Test Summary ==="  
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All external tool integration tests passed!"
        stop 0
    else
        print *, "Some external tool integration tests failed!"
        stop 1
    end if

contains

    subroutine test_pure_fortran_tool_compilation()
        ! Given: A sample external Fortran program using fortfront
        ! When: The program is compiled against libfortfront.a
        ! Then: Compilation should succeed with no C dependencies
        integer :: exit_code, unit_num
        logical :: compilation_success
        
        call test_start("Pure Fortran tool compilation")
        
        ! Create sample external tool code
        open(newunit=unit_num, file='sample_fluff_tool.f90', status='replace')
        write(unit_num, '(A)') 'program sample_fluff_tool'
        write(unit_num, '(A)') '    use fortfront_core           ! Direct Fortran module usage'
        write(unit_num, '(A)') '    use fortfront_semantic       ! No C API needed'
        write(unit_num, '(A)') '    use fortfront_ast_arena      ! Pure Fortran interfaces'
        write(unit_num, '(A)') '    implicit none'
        write(unit_num, '(A)') '    '
        write(unit_num, '(A)') '    character(len=*), parameter :: source = "print *, ''Hello''"'
        write(unit_num, '(A)') '    ! ast = fortfront_parse(source)  ! Pure Fortran function calls'
        write(unit_num, '(A)') '    ! call fluff_analyze(ast)        ! All Fortran, all the time'
        write(unit_num, '(A)') '    print *, "Sample fluff tool would work here"'
        write(unit_num, '(A)') 'end program sample_fluff_tool'
        close(unit_num)
        
        ! Try to compile with static linking (should fail in RED phase)
        call execute_command_line( &
            'gfortran -static sample_fluff_tool.f90 libfortfront.a -o sample_fluff 2>compile_error.txt', &
            exitstat=exit_code)
        
        compilation_success = (exit_code == 0)
        
        ! Clean up test files
        call execute_command_line('rm -f sample_external_tool.f90 sample_external_tool', &
                                  exitstat=exit_code, wait=.true.)
        
        call test_result(compilation_success)
        if (compilation_success) then
            print *, "  SUCCESS: External tool compiled successfully"
        else
            print *, "  FAILED: Compilation error"
        end if
    end subroutine test_pure_fortran_tool_compilation

    subroutine test_static_linking_works()
        ! Given: A valid external Fortran program
        ! When: Static linking is performed with libfortfront.a
        ! Then: Resulting executable should be self-contained
        integer :: exit_code, unit_num
        logical :: linking_success, exe_exists
        
        call test_start("Static linking produces self-contained executable")
        
        ! Create minimal test program using fortfront
        open(newunit=unit_num, file='minimal_tool.f90', status='replace')
        write(unit_num, '(A)') 'program minimal_tool'
        write(unit_num, '(A)') '    use fortfront_external_interface'
        write(unit_num, '(A)') '    implicit none'
        write(unit_num, '(A)') '    type(fortfront_result_t) :: result'
        write(unit_num, '(A)') '    result = fortfront_transform_source("program test; end program")'
        write(unit_num, '(A)') '    if (result%success) print *, "OK"'
        write(unit_num, '(A)') 'end program minimal_tool'
        close(unit_num)
        
        ! Test linking with library
        call execute_command_line( &
            'gfortran -I fortfront_modules/ minimal_tool.f90 libfortfront.a ' // &
            '-o minimal_tool_exe 2>/dev/null', &
            exitstat=exit_code, wait=.true.)
        
        inquire(file='minimal_tool_exe', exist=exe_exists)
        linking_success = (exit_code == 0 .and. exe_exists)
        
        ! Clean up
        call execute_command_line('rm -f minimal_tool.f90 minimal_tool_exe', &
            exitstat=exit_code, wait=.true.)
        
        call test_result(linking_success)
        if (linking_success) then
            print *, "  SUCCESS: Static linking works"
        else
            print *, "  FAILED: Static linking failed"
        end if
    end subroutine test_static_linking_works

    subroutine test_module_usage_from_external()
        ! Given: An external program with `use` statements for fortfront modules
        ! When: The program attempts to access fortfront functionality
        ! Then: All module interfaces should be accessible
        integer :: unit_num, exit_code
        logical :: module_access_success
        
        call test_start("Module interfaces accessible from external program")
        
        ! Create test program with explicit module usage
        open(newunit=unit_num, file='module_test_tool.f90', status='replace')
        write(unit_num, '(A)') 'program module_test_tool'
        write(unit_num, '(A)') '    use error_handling'
        write(unit_num, '(A)') '    use lexer_core'
        write(unit_num, '(A)') '    use ast_core'
        write(unit_num, '(A)') '    implicit none'
        write(unit_num, '(A)') '    '
        write(unit_num, '(A)') '    print *, "Module interfaces accessible"'
        write(unit_num, '(A)') 'end program module_test_tool'
        close(unit_num)
        
        ! Test module compilation with module path
        call execute_command_line( &
            'gfortran -I fortfront_modules/ -c module_test_tool.f90 2>/dev/null', &
            exitstat=exit_code, wait=.true.)
        
        module_access_success = (exit_code == 0)
        
        call execute_command_line('rm -f module_test_tool.f90 module_test_tool.o', &
            exitstat=exit_code, wait=.true.)
        
        call test_result(module_access_success)
        if (module_access_success) then
            print *, "  SUCCESS: Modules accessible from external program"
        else
            print *, "  FAILED: Modules not accessible"
        end if
    end subroutine test_module_usage_from_external

    subroutine test_self_contained_executable()  
        ! Given: A successfully linked external tool
        ! When: The executable is analyzed for dependencies
        ! Then: It should only depend on system libraries (no external fortfront deps)
        integer :: exit_code, unit_num
        logical :: is_self_contained, exe_exists
        
        call test_start("Linked executable is self-contained")
        
        ! Create and build a test executable
        open(newunit=unit_num, file='test_contained.f90', status='replace')
        write(unit_num, '(A)') 'program test_contained'
        write(unit_num, '(A)') '    use fortfront_external_interface'
        write(unit_num, '(A)') '    implicit none'
        write(unit_num, '(A)') '    print *, "Test"'
        write(unit_num, '(A)') 'end program test_contained'
        close(unit_num)
        
        ! Build the executable
        call execute_command_line( &
            'gfortran -I fortfront_modules/ test_contained.f90 libfortfront.a ' // &
            '-o test_contained 2>/dev/null', &
            exitstat=exit_code, wait=.true.)
        
        inquire(file='test_contained', exist=exe_exists)
        
        if (exe_exists) then
            ! Check that it doesn't require external fortfront libraries
            call execute_command_line( &
                'ldd test_contained 2>/dev/null | grep -q fortfront', &
                exitstat=exit_code, wait=.true.)
            ! exit_code should be non-zero (grep fails) if no fortfront deps
            is_self_contained = (exit_code /= 0)
        else
            is_self_contained = .false.
        end if
        
        ! Clean up
        call execute_command_line('rm -f test_contained.f90 test_contained', &
            wait=.true.)
        
        call test_result(is_self_contained)
        if (is_self_contained) then
            print *, "  SUCCESS: Executable is self-contained"
        else
            print *, "  FAILED: Executable has external dependencies"
        end if
    end subroutine test_self_contained_executable

    subroutine test_multiple_tool_integration()
        ! Given: Multiple external tools using libfortfront.a  
        ! When: Each tool is built independently
        ! Then: All tools should build and link successfully
        integer :: i, exit_code, unit_num, build_count
        logical :: all_tools_build
        character(len=20), dimension(3) :: tool_names
        
        call test_start("Multiple tools can integrate simultaneously")
        
        tool_names = [ "fluff_tool    ", "fortrun_tool  ", "custom_tool   " ]
        
        build_count = 0
        ! Create and build multiple test tools
        do i = 1, size(tool_names)
            open(newunit=unit_num, file=trim(tool_names(i))//'.f90', status='replace')
            write(unit_num, '(A)') 'program '//trim(tool_names(i))
            write(unit_num, '(A)') '    use fortfront_external_interface'
            write(unit_num, '(A)') '    implicit none'
            write(unit_num, '(A)') '    type(fortfront_result_t) :: result'
            write(unit_num, '(A)') '    result = fortfront_transform_source("program test; end")'
            write(unit_num, '(A)') '    print *, "Tool: '//trim(tool_names(i))//'"'
            write(unit_num, '(A)') 'end program '//trim(tool_names(i))
            close(unit_num)
            
            ! Try to build each tool
            call execute_command_line( &
                'gfortran -I fortfront_modules/ '//trim(tool_names(i))//'.f90 ' // &
                'libfortfront.a -o '//trim(tool_names(i))//' 2>/dev/null', &
                exitstat=exit_code, wait=.true.)
            
            if (exit_code == 0) build_count = build_count + 1
        end do
        
        all_tools_build = (build_count == size(tool_names))
        
        ! Clean up test files
        do i = 1, size(tool_names)
            call execute_command_line('rm -f '//trim(tool_names(i))//'.f90 '//trim(tool_names(i)), &
                exitstat=exit_code, wait=.true.)
        end do
        
        call test_result(all_tools_build)
        if (all_tools_build) then
            write(*, '(A,I0,A)') "  SUCCESS: All ", size(tool_names), " tools built"
        else
            write(*, '(A,I0,A,I0,A)') "  FAILED: Only ", build_count, "/", size(tool_names), " tools built"
        end if
    end subroutine test_multiple_tool_integration

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

end program test_external_tool_integration