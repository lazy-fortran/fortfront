program test_dependencies_interface
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t
    use semantic_analyzer_base, only: semantic_analyzer_t
    implicit none
    
    logical :: test_passed = .true.
    
    ! Test that the new interface compiles and types can be instantiated
    call test_interface_compilation()
    
    if (test_passed) then
        print *, "TEST PASSED: dependencies interface compiles correctly"
    else
        print *, "TEST FAILED: dependencies interface compilation"
        stop 1
    end if

contains

    subroutine test_interface_compilation()
        type(symbol_analyzer_t) :: symbol_analyzer
        type(type_analyzer_t) :: type_analyzer
        class(semantic_analyzer_t), allocatable :: base_analyzer
        
        ! Test that we can instantiate analyzers with new interface
        allocate(symbol_analyzer_t :: base_analyzer)
        
        ! Test that analyzers can be assigned
        base_analyzer = symbol_analyzer
        
        ! Basic check that the interface exists
        print *, "SUCCESS: Interface compiles and types work correctly"
    end subroutine

end program test_dependencies_interface