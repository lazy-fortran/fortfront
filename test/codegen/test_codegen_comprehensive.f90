program test_codegen_comprehensive
    use codegen_core, only: generate_code_from_arena
    use ast_core
    use ast_factory
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Comprehensive Codegen Tests ==='
    print *

    ! Test basic code generation
    print *, 'Testing code generation from AST...'
    if (.not. test_code_generation()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All codegen tests passed!'
        stop 0
    else
        print *, 'Some codegen tests failed!'
        stop 1
    end if

contains

    function test_code_generation() result(passed)
        logical :: passed
        integer :: prog_id
        character(len=:), allocatable :: code
        
        passed = .true.
        
        ! Create a simple AST
        prog_id = push_program("test")
        
        ! Generate code - just test that it works
        code = generate_code_from_arena()
        
        ! Basic check - should have some content
        if (len(code) == 0) then
            print *, '  FAILED: No code generated'
            passed = .false.
        else
            print *, '  Generated', len(code), 'characters of code'
        end if
        
        if (passed) print *, '  PASSED: Code generation from AST'
    end function

end program test_codegen_comprehensive