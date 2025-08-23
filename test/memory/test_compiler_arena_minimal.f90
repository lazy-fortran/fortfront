program test_compiler_arena_minimal
    ! Minimal test for compiler_arena in frontend
    
    use frontend
    implicit none
    
    type(compilation_options_t) :: options
    character(len=:), allocatable :: error_msg
    character(len=256) :: temp_file
    integer :: unit
    
    print *, "=== Minimal Compiler Arena Frontend Test ==="
    
    ! Create a temporary source file
    temp_file = "test_minimal.f90"
    open(newunit=unit, file=temp_file, status='replace')
    write(unit, '(A)') "program test"
    write(unit, '(A)') "    implicit none"
    write(unit, '(A)') "    integer :: x = 42"
    write(unit, '(A)') "end program test"
    close(unit)
    
    print *, "Created test file:", trim(temp_file)
    
    ! Initialize options with default values
    options%debug_tokens = .false.
    options%debug_ast = .false.
    options%debug_semantic = .false.
    options%debug_standardize = .false.
    options%debug_codegen = .false.
    options%use_unified_arena = .true.  ! Enable unified arena
    
    print *, "Options initialized, use_unified_arena =", options%use_unified_arena
    
    ! Allocate error_msg
    allocate(character(len=0) :: error_msg)
    
    ! Call compile_source
    print *, "Calling compile_source..."
    call compile_source(temp_file, options, error_msg)
    
    if (error_msg == "") then
        print *, "PASS: Compilation succeeded"
    else
        print *, "FAIL: Compilation failed with:", error_msg
        stop 1
    end if
    
    ! Clean up
    call execute_command_line("rm -f " // temp_file, wait=.true.)
    
    print *, "Test completed successfully!"
    
end program test_compiler_arena_minimal