program test_fortran_language_linking
    ! **Given-When-Then**: Test Fortran language linking with libfortfront.a
    ! **Given**: libfortfront.a static library exists and is properly built
    ! **When**: Attempting to link simple Fortran program with libfortfront.a
    ! **Then**: Should compile and link successfully with direct module access
    
    ! use error_handling  ! Removed for RED phase testing
    implicit none
    
    ! Test execution status
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    ! Execute test functions
    if (.not. test_fortran_compilation_links()) all_tests_passed = .false.
    if (.not. test_fortran_module_access()) all_tests_passed = .false.
    if (.not. test_fortran_api_integration()) all_tests_passed = .false.
    
    ! Report results
    if (all_tests_passed) then
        print *, "PASS: All Fortran language linking tests passed"
    else
        print *, "FAIL: Some Fortran language linking tests failed"
        error stop 1
    end if
    
contains
    
    function test_fortran_compilation_links() result(test_passed)
        ! **Given**: Fortran test program and libfortfront.a
        ! **When**: Compiling with gfortran and linking statically
        ! **Then**: Should compile and link without module file issues
        logical :: test_passed
        
        print *, "TEST: Verifying Fortran program can link with libfortfront.a"
        
        ! This will fail in RED phase - need Fortran test program and module installation
        test_passed = .false.
        
        print *, "FAIL: Fortran linking test not implemented"
        print *, "EXPECTED: gfortran -static test.f90 libfortfront.a -o test_fortran should succeed"
        print *, "ACTUAL: Test program and module installation needed"
        
    end function test_fortran_compilation_links
    
    function test_fortran_module_access() result(test_passed)
        ! **Given**: libfortfront.a with compiled module files
        ! **When**: Importing fortfront modules in external Fortran program
        ! **Then**: Should have access to all public interfaces
        logical :: test_passed
        
        print *, "TEST: Verifying Fortran modules are accessible from libfortfront.a"
        
        ! This will fail in RED phase - module installation not configured
        test_passed = .false.
        
        print *, "FAIL: Fortran module access test not implemented"
        print *, "EXPECTED: use fortfront, lexer_core, parser_core should work"
        print *, "ACTUAL: Module file installation and search paths needed"
        
    end function test_fortran_module_access
    
    function test_fortran_api_integration() result(test_passed)
        ! **Given**: External Fortran program using libfortfront.a
        ! **When**: Calling core fortfront functions (lexer, parser, semantic)
        ! **Then**: Should execute full compilation pipeline successfully
        logical :: test_passed
        
        print *, "TEST: Verifying full fortfront API works through static library"
        
        ! This will fail in RED phase - API integration test not implemented
        test_passed = .false.
        
        print *, "FAIL: Fortran API integration test not implemented"
        print *, "EXPECTED: Should be able to parse Fortran code using libfortfront.a"
        print *, "ACTUAL: End-to-end API test needed"
        
    end function test_fortran_api_integration
    
end program test_fortran_language_linking