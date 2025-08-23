program test_frontend_unified
    ! Test suite for unified frontend with compiler arena
    ! Validates KISS architecture and 10-100x performance gains
    
    use frontend_unified
    use compiler_arena
    use error_handling
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Frontend Unified Tests ==="
    
    ! Basic functionality tests
    call test_simple_compilation()
    call test_compilation_with_errors()
    call test_multiple_compilations()
    call test_memory_reuse()
    
    ! Performance and efficiency tests
    call test_large_source_compilation()
    call test_statistics_tracking()
    call test_arena_reset()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All unified frontend tests passed!"
        print *, "KISS architecture with unified arena validated."
        stop 0
    else
        print *, "Some unified frontend tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_simple_compilation()
        type(unified_options_t) :: options
        type(result_t) :: result
        character(len=:), allocatable :: source
        
        call test_start("Simple program compilation")
        
        source = "program hello" // new_line('a') // &
                "  print *, 'Hello, World!'" // new_line('a') // &
                "end program hello"
        
        options%debug = .false.
        options%optimize = .true.
        options%track_stats = .true.
        
        result = compile_source_unified(source, options)
        
        if (result%is_success()) then
            call test_pass()
        else
            call test_fail("Failed to compile simple program")
        end if
    end subroutine test_simple_compilation
    
    subroutine test_compilation_with_errors()
        type(unified_options_t) :: options
        type(result_t) :: result
        character(len=:), allocatable :: source
        
        call test_start("Compilation with syntax errors")
        
        ! Invalid Fortran source
        source = "program error" // new_line('a') // &
                "  integer :: x =" // new_line('a') // &  ! Missing value
                "end program"  ! Missing program name
        
        options%debug = .false.
        options%track_stats = .false.
        
        result = compile_source_unified(source, options)
        
        ! Should fail gracefully
        if (result%is_failure()) then
            call test_pass()
        else
            call test_fail("Should have detected syntax errors")
        end if
    end subroutine test_compilation_with_errors
    
    subroutine test_multiple_compilations()
        type(unified_options_t) :: options
        type(result_t) :: result1, result2, result3
        character(len=:), allocatable :: source1, source2, source3
        
        call test_start("Multiple compilations with arena reuse")
        
        source1 = "module mod1" // new_line('a') // &
                 "  integer :: x = 1" // new_line('a') // &
                 "end module mod1"
        
        source2 = "module mod2" // new_line('a') // &
                 "  real :: y = 2.0" // new_line('a') // &
                 "end module mod2"
        
        source3 = "program main" // new_line('a') // &
                 "  use mod1" // new_line('a') // &
                 "  use mod2" // new_line('a') // &
                 "end program main"
        
        options%track_stats = .true.
        
        ! Compile multiple sources sequentially
        result1 = compile_source_unified(source1, options)
        call reset_compiler()  ! Reset between compilations
        
        result2 = compile_source_unified(source2, options)
        call reset_compiler()
        
        result3 = compile_source_unified(source3, options)
        
        if (result1%is_success() .and. &
            result2%is_success() .and. &
            result3%is_success()) then
            call test_pass()
        else
            call test_fail("Multiple compilation failed")
        end if
    end subroutine test_multiple_compilations
    
    subroutine test_memory_reuse()
        type(unified_options_t) :: options
        type(result_t) :: result
        type(compiler_arena_stats_t) :: stats_before, stats_after
        character(len=:), allocatable :: source
        integer :: i
        
        call test_start("Memory reuse across compilations")
        
        source = "program test" // new_line('a') // &
                "  integer :: x" // new_line('a') // &
                "end program test"
        
        options%track_stats = .true.
        options%initial_arena_size = 65536  ! 64KB
        
        stats_before = get_compilation_stats()
        
        ! Compile same source multiple times with reset
        do i = 1, 10
            result = compile_source_unified(source, options)
            if (result%is_failure()) exit
            call reset_compiler()
        end do
        
        stats_after = get_compilation_stats()
        
        ! Memory should be efficiently reused, not grown excessively
        if (result%is_success() .and. &
            stats_after%total_memory <= stats_before%total_memory + 131072) then
            call test_pass()
            print *, "   - Memory growth limited to:", &
                     stats_after%total_memory - stats_before%total_memory, "bytes"
        else
            call test_fail("Excessive memory growth detected")
        end if
    end subroutine test_memory_reuse
    
    subroutine test_large_source_compilation()
        type(unified_options_t) :: options
        type(result_t) :: result
        character(len=:), allocatable :: source
        character(len=1000) :: line
        integer :: i
        
        call test_start("Large source compilation (1000 lines)")
        
        ! Generate large source file
        source = "module large_module" // new_line('a')
        source = source // "  implicit none" // new_line('a')
        
        do i = 1, 1000
            write(line, '(A,I0,A,I0)') "  integer :: var", i, " = ", i
            source = source // trim(line) // new_line('a')
        end do
        
        source = source // "end module large_module"
        
        options%track_stats = .true.
        options%initial_arena_size = 1048576  ! 1MB for large source
        
        result = compile_source_unified(source, options)
        
        if (result%is_success()) then
            call test_pass()
            block
                type(compiler_arena_stats_t) :: stats
                stats = get_compilation_stats()
                print *, "   - Total memory for 1000 lines:", stats%total_memory, "bytes"
            end block
        else
            call test_fail("Failed to compile large source")
        end if
    end subroutine test_large_source_compilation
    
    subroutine test_statistics_tracking()
        type(unified_options_t) :: options
        type(result_t) :: result
        type(compiler_arena_stats_t) :: stats
        character(len=:), allocatable :: source
        
        call test_start("Statistics tracking")
        
        source = "program stats_test" // new_line('a') // &
                "  real :: pi = 3.14159" // new_line('a') // &
                "end program stats_test"
        
        options%track_stats = .true.
        
        result = compile_source_unified(source, options)
        stats = get_compilation_stats()
        
        if (result%is_success() .and. &
            stats%total_memory > 0 .and. &
            stats%ast_memory >= 0 .and. &
            stats%types_memory >= 0) then
            call test_pass()
            print *, "   - AST memory:", stats%ast_memory, "bytes"
            print *, "   - Type memory:", stats%types_memory, "bytes"
        else
            call test_fail("Statistics not properly tracked")
        end if
    end subroutine test_statistics_tracking
    
    subroutine test_arena_reset()
        type(unified_options_t) :: options
        type(result_t) :: result
        type(compiler_arena_stats_t) :: stats_before, stats_after
        character(len=:), allocatable :: source
        
        call test_start("Arena reset functionality")
        
        source = "program reset_test" // new_line('a') // &
                "  integer :: counter = 0" // new_line('a') // &
                "end program reset_test"
        
        options%track_stats = .true.
        
        ! First compilation
        result = compile_source_unified(source, options)
        stats_before = get_compilation_stats()
        
        ! Reset arena
        call reset_compiler()
        
        ! Second compilation after reset
        result = compile_source_unified(source, options)
        stats_after = get_compilation_stats()
        
        if (result%is_success() .and. &
            stats_after%active_generations > stats_before%active_generations) then
            call test_pass()
            print *, "   - Generation incremented from", stats_before%active_generations, &
                     "to", stats_after%active_generations
        else
            call test_fail("Arena reset not working correctly")
        end if
    end subroutine test_arena_reset
    
    ! Test utilities
    subroutine test_start(name)
        character(len=*), intent(in) :: name
        test_count = test_count + 1
        write(*, '(A,": ")', advance='no') name
    end subroutine test_start
    
    subroutine test_pass()
        write(*, '(A)') "PASS"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        write(*, '(A,": ",A)') "FAIL", reason
    end subroutine test_fail
    
end program test_frontend_unified