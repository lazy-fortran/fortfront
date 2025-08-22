program fortran_test_program
    ! **Given-When-Then**: Simple Fortran program to verify linking with libfortfront.a
    ! **Given**: libfortfront.a exists and provides direct Fortran module interface
    ! **When**: Compiling this program with gfortran -static
    ! **Then**: Should compile, link, and run with direct module access
    
    ! NOTE: These use statements will fail in RED phase since module installation not configured yet
    ! The actual module installation will need to be implemented in the GREEN phase
    
    use fortfront
    use lexer_core
    use parser_core
    use semantic_analyzer
    use codegen_core
    
    implicit none
    
    character(len=:), allocatable :: test_source
    logical :: success
    
    print *, "Testing libfortfront.a from Fortran..."
    
    ! This will fail in RED phase - direct Fortran interface not properly configured
    test_source = "program hello" // new_line('a') // &
                  "  print *, 'Hello World'" // new_line('a') // &
                  "end program"
    
    ! Test full compilation pipeline
    call test_fortfront_pipeline(test_source, success)
    
    if (success) then
        print *, "PASS: Fortran program successfully used libfortfront.a"
    else
        print *, "FAIL: Could not use fortfront library from Fortran"
        error stop 1
    end if
    
contains
    
    subroutine test_fortfront_pipeline(source_code, success)
        character(len=*), intent(in) :: source_code
        logical, intent(out) :: success
        
        ! This will fail in RED phase - direct API calls not configured
        success = .false.
        
        print *, "FAIL: Direct Fortran API not implemented yet"
        print *, "EXPECTED: Should be able to call lexer, parser, semantic, codegen"
        print *, "ACTUAL: Module interface and API design needed"
        
    end subroutine test_fortfront_pipeline
    
end program fortran_test_program