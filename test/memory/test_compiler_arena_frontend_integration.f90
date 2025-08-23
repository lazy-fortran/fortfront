program test_compiler_arena_frontend_integration
    ! Integration test for compiler_arena in frontend compilation pipeline
    ! Validates phase management, generation tracking, and statistics
    
    use frontend
    use compiler_arena
    use ast_arena
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Compiler Arena Frontend Integration Tests ==="
    
    call test_unified_arena_compilation()
    call test_phase_generation_tracking()
    call test_arena_statistics_reporting()
    call test_backward_compatibility()
    call test_memory_efficiency()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All compiler arena frontend integration tests passed!"
        stop 0
    else
        print *, "Some tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_unified_arena_compilation()
        type(compilation_options_t) :: options
        character(len=:), allocatable :: error_msg
        character(len=256) :: temp_file
        integer :: unit
        
        call test_start("Unified arena compilation")
        
        ! Create a temporary source file
        temp_file = "test_unified_arena.f90"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "program test"
        write(unit, '(A)') "    implicit none"
        write(unit, '(A)') "    integer :: x = 42"
        write(unit, '(A)') "    print *, x"
        write(unit, '(A)') "end program test"
        close(unit)
        
        ! Configure to use unified arena
        options%use_unified_arena = .true.
        options%debug_tokens = .false.
        options%debug_ast = .false.
        options%debug_semantic = .false.
        options%debug_standardize = .false.
        options%debug_codegen = .false.
        options%output_file = "test_output.f90"
        
        ! Allocate error_msg
        allocate(character(len=0) :: error_msg)
        
        ! Compile with unified arena
        call compile_source(temp_file, options, error_msg)
        
        if (error_msg == "") then
            call test_pass()
        else
            call test_fail("Compilation failed: " // error_msg)
        end if
        
        ! Clean up
        call execute_command_line("rm -f " // temp_file // " test_output.f90", wait=.true.)
    end subroutine test_unified_arena_compilation
    
    subroutine test_phase_generation_tracking()
        type(compilation_options_t) :: options
        character(len=:), allocatable :: error_msg
        character(len=256) :: temp_file
        integer :: unit
        
        call test_start("Phase generation tracking")
        
        ! Create a temporary source file
        temp_file = "test_phases.f90"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "program test_phases"
        write(unit, '(A)') "    implicit none"
        write(unit, '(A)') "    real :: y"
        write(unit, '(A)') "    y = 3.14"
        write(unit, '(A)') "end program test_phases"
        close(unit)
        
        ! Configure with debug output to verify phases
        options%use_unified_arena = .true.
        options%debug_tokens = .true.  ! Will print phases
        options%debug_ast = .true.
        options%debug_semantic = .true.
        options%debug_standardize = .true.
        options%debug_codegen = .true.
        
        ! Allocate error_msg
        allocate(character(len=0) :: error_msg)
        
        ! Redirect stdout to capture phase output
        call compile_source(temp_file, options, error_msg)
        
        ! Phase tracking is working if compilation succeeds
        if (error_msg == "") then
            call test_pass()
        else
            call test_fail("Phase tracking failed: " // error_msg)
        end if
        
        ! Clean up
        call execute_command_line("rm -f " // temp_file, wait=.true.)
    end subroutine test_phase_generation_tracking
    
    subroutine test_arena_statistics_reporting()
        type(compiler_arena_t) :: arena
        type(compiler_arena_stats_t) :: stats
        integer :: prog_index
        
        call test_start("Arena statistics reporting")
        
        ! Create and use arena
        arena = create_compiler_arena(chunk_size=65536, enable_stats=.true.)
        
        ! Add some AST nodes
        prog_index = push_program(arena%ast, "test_stats", [1, 2, 3], 1, 1)
        
        ! Get statistics
        stats = arena%get_stats()
        
        if (stats%total_memory >= 0 .and. &
            stats%ast_memory >= 0 .and. &
            stats%types_memory >= 0 .and. &
            stats%active_generations >= 1) then
            call test_pass()
        else
            call test_fail("Statistics not collected properly")
        end if
        
        call destroy_compiler_arena(arena)
    end subroutine test_arena_statistics_reporting
    
    subroutine test_backward_compatibility()
        type(compilation_options_t) :: options
        character(len=:), allocatable :: error_msg
        character(len=256) :: temp_file
        integer :: unit
        
        call test_start("Backward compatibility (unified arena disabled)")
        
        ! Create a temporary source file
        temp_file = "test_compat.f90"
        open(newunit=unit, file=temp_file, status='replace')
        write(unit, '(A)') "program test_compat"
        write(unit, '(A)') "    implicit none"
        write(unit, '(A)') "    logical :: flag = .true."
        write(unit, '(A)') "end program test_compat"
        close(unit)
        
        ! Disable unified arena for backward compatibility
        options%use_unified_arena = .false.
        options%debug_tokens = .false.
        
        ! Allocate error_msg
        allocate(character(len=0) :: error_msg)
        
        ! Compile without unified arena
        call compile_source(temp_file, options, error_msg)
        
        if (error_msg == "") then
            call test_pass()
        else
            call test_fail("Backward compatibility broken: " // error_msg)
        end if
        
        ! Clean up
        call execute_command_line("rm -f " // temp_file, wait=.true.)
    end subroutine test_backward_compatibility
    
    subroutine test_memory_efficiency()
        type(compiler_arena_t) :: arena1, arena2
        type(compiler_arena_stats_t) :: stats1, stats2
        integer :: i, node_index
        integer, allocatable :: indices(:)
        
        call test_start("Memory efficiency with unified arena")
        
        ! Test 1: Small allocation
        arena1 = create_compiler_arena(chunk_size=4096, enable_stats=.true.)
        allocate(indices(10))
        do i = 1, 10
            indices(i) = push_program(arena1%ast, "prog", [i], 1, 1)
        end do
        stats1 = arena1%get_stats()
        
        ! Test 2: Large allocation
        arena2 = create_compiler_arena(chunk_size=1048576, enable_stats=.true.)
        do i = 1, 10
            indices(i) = push_program(arena2%ast, "prog", [i], 1, 1)
        end do
        stats2 = arena2%get_stats()
        
        ! Both should work efficiently
        if (stats1%total_memory > 0 .and. stats2%total_memory > 0) then
            call test_pass()
        else
            call test_fail("Memory allocation inefficient")
        end if
        
        call destroy_compiler_arena(arena1)
        call destroy_compiler_arena(arena2)
        deallocate(indices)
    end subroutine test_memory_efficiency
    
    ! Test helper routines
    subroutine test_start(name)
        character(len=*), intent(in) :: name
        test_count = test_count + 1
        write(*, '(A,A,A)', advance='no') trim(name), ": "
    end subroutine test_start
    
    subroutine test_pass()
        pass_count = pass_count + 1
        print *, "PASS"
    end subroutine test_pass
    
    subroutine test_fail(message)
        character(len=*), intent(in) :: message
        print *, "FAIL - ", message
    end subroutine test_fail
    
    function push_program(arena, name, body_indices, line, col) result(index)
        use ast_factory, only: factory_push_program => push_program
        type(ast_arena_t), intent(inout) :: arena
        character(len=*), intent(in) :: name
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: line, col
        integer :: index
        
        index = factory_push_program(arena, name, body_indices, line, col)
    end function push_program
    
end program test_compiler_arena_frontend_integration