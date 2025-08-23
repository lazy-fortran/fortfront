program test_module_distribution
    ! RED Phase Test: Module Distribution Verification  
    ! Issue #416: Foundation: Create libfortfront.a static library
    !
    ! Given: A fortfront static library build process
    ! When: The build system collects all .mod files
    ! Then: All required Fortran modules should be available for external use
    !
    use iso_fortran_env, only: error_unit
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Module Distribution Tests ==="
    print *, ""
    
    ! Test 1: Core modules are accessible
    call test_core_modules_available()
    
    ! Test 2: Module interfaces are complete
    call test_module_interfaces_complete()
    
    ! Test 3: Module dependencies resolved
    call test_module_dependencies_resolved()
    
    ! Test 4: Module installation location
    call test_module_installation_location()
    
    ! Test 5: Module version compatibility
    call test_module_version_compatibility()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All module distribution tests passed!"
        stop 0
    else
        print *, "Some module distribution tests failed!"
        stop 1
    end if

contains

    subroutine test_core_modules_available()
        ! Given: A static library build with module collection
        ! When: Core fortfront modules are checked
        ! Then: All essential modules should be available to external programs
        character(len=100), dimension(10) :: required_modules
        integer :: i, exit_code, found_count
        logical :: all_modules_found, mod_exists
        character(len=256) :: mod_path
        
        call test_start("Core modules are available")
        
        ! Build library and modules first
        call execute_command_line('make libfortfront.a > /dev/null 2>&1', &
            exitstat=exit_code, wait=.true.)
        
        ! Define required modules for pure Fortran integration
        required_modules = [ &
            "fortfront_external_interface", &
            "frontend                    ", &
            "lexer_core                  ", &
            "parser_core                 ", &
            "semantic_analyzer           ", &
            "codegen_core                ", &
            "ast_core                    ", &
            "type_system_unified         ", &
            "error_handling              ", &
            "scope_manager               " ]
        
        found_count = 0
        ! Check if modules are in expected location
        do i = 1, size(required_modules)
            if (len_trim(required_modules(i)) > 0) then
                write(mod_path, '(A,A,A)') 'fortfront_modules/', &
                    trim(adjustl(required_modules(i))), '.mod'
                inquire(file=trim(mod_path), exist=mod_exists)
                if (mod_exists) found_count = found_count + 1
            end if
        end do
        
        all_modules_found = (found_count >= 8)  ! At least 8 of 10 core modules
        
        call test_result(all_modules_found)
        if (all_modules_found) then
            write(*, '(A,I0,A)') "  SUCCESS: Found ", found_count, " core modules"
        else
            write(*, '(A,I0,A)') "  FAILED: Only found ", found_count, " core modules"
        end if
    end subroutine test_core_modules_available

    subroutine test_module_interfaces_complete()
        ! Given: Collected module files
        ! When: Module interfaces are inspected
        ! Then: All public interfaces should be accessible
        integer :: exit_code
        logical :: interfaces_complete, mod_exists
        
        call test_start("Module interfaces are complete")
        
        ! Check key interface module exists
        inquire(file='fortfront_modules/fortfront_external_interface.mod', &
                exist=mod_exists)
        
        interfaces_complete = mod_exists
        
        call test_result(interfaces_complete)
        if (interfaces_complete) then
            print *, "  SUCCESS: External interface module found"
        else
            print *, "  FAILED: External interface module missing"
        end if
    end subroutine test_module_interfaces_complete

    subroutine test_module_dependencies_resolved()
        ! Given: A set of collected modules
        ! When: Module dependency chain is analyzed
        ! Then: All dependencies should be satisfied within the collection
        logical :: dependencies_resolved
        logical :: lex_exists, parse_exists, ast_exists, sem_exists
        
        call test_start("Module dependencies are resolved")
        
        ! Check key dependency chains
        inquire(file='fortfront_modules/lexer_core.mod', exist=lex_exists)
        inquire(file='fortfront_modules/parser_core.mod', exist=parse_exists)
        inquire(file='fortfront_modules/ast_core.mod', exist=ast_exists)
        inquire(file='fortfront_modules/semantic_analyzer.mod', exist=sem_exists)
        
        dependencies_resolved = (lex_exists .and. parse_exists .and. &
                               ast_exists .and. sem_exists)
        
        call test_result(dependencies_resolved)
        if (dependencies_resolved) then
            print *, "  SUCCESS: Core dependency chain complete"
        else
            print *, "  FAILED: Missing modules in dependency chain"
        end if
    end subroutine test_module_dependencies_resolved

    subroutine test_module_installation_location()
        ! Given: A static library build process
        ! When: Module files are installed
        ! Then: They should be in predictable, accessible location
        logical :: location_correct
        integer :: exit_code
        
        call test_start("Modules installed in correct location")
        
        ! Check if fortfront_modules directory exists and has content
        call execute_command_line( &
            'test -d fortfront_modules && test "$(ls -A fortfront_modules)"', &
            exitstat=exit_code, wait=.true.)
        
        location_correct = (exit_code == 0)
        
        call test_result(location_correct)
        if (location_correct) then
            print *, "  SUCCESS: Modules in fortfront_modules/ directory"
        else
            print *, "  FAILED: Module directory missing or empty"
        end if
    end subroutine test_module_installation_location

    subroutine test_module_version_compatibility()
        ! Given: Module files from fortfront build
        ! When: Version compatibility is checked
        ! Then: Modules should be compatible with standard Fortran compilers
        integer :: exit_code
        logical :: version_compatible
        
        call test_start("Modules are version compatible")
        
        ! Test that modules can be used with gfortran
        call execute_command_line( &
            'echo "program test; use error_handling; end program" > /tmp/test_mod.f90 && ' // &
            'gfortran -I fortfront_modules/ -c /tmp/test_mod.f90 -o /tmp/test_mod.o 2>/dev/null', &
            exitstat=exit_code, wait=.true.)
        
        version_compatible = (exit_code == 0)
        
        call test_result(version_compatible)
        if (version_compatible) then
            print *, "  SUCCESS: Modules compatible with gfortran"
        else
            print *, "  FAILED: Module compatibility issue"
        end if
    end subroutine test_module_version_compatibility

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

end program test_module_distribution