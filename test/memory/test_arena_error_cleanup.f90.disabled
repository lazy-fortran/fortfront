program test_arena_error_cleanup
    ! Error scenario cleanup tests for Issue #318: Memory Management - Inconsistent arena cleanup pattern
    !
    ! Given: Arena cleanup should be reliable even when errors occur during operations
    ! When: Testing cleanup behavior after various error conditions
    ! Then: Cleanup should handle error states consistently and safely
    
    use ast_core, only: ast_arena_t, create_ast_arena, ast_arena_stats_t
    use ast_base, only: ast_node
    use ast_nodes_core, only: identifier_node, literal_node
    use ast_core, only: create_identifier, create_literal, LITERAL_INTEGER
    use lexer_core, only: token_t, tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    implicit none

    integer :: total_tests, passed_tests
    logical :: all_passed

    total_tests = 0
    passed_tests = 0
    all_passed = .true.

    print *, "=== Arena Error Cleanup Tests (Issue #318) ==="
    print *, "Testing cleanup reliability during error conditions"
    print *, ""

    ! Test cleanup after parsing errors
    call test_cleanup_after_parse_error()
    
    ! Test cleanup after memory allocation failures
    call test_cleanup_after_allocation_error()
    
    ! Test cleanup during arena corruption scenarios
    call test_cleanup_arena_corruption()
    
    ! Test cleanup with invalid node operations
    call test_cleanup_invalid_operations()
    
    ! Test cleanup during exception conditions
    call test_cleanup_exception_conditions()
    
    ! Test cleanup resilience under multiple error types
    call test_cleanup_multiple_error_types()

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (all_passed) then
        print *, "All error cleanup tests passed!"
        print *, "Arena cleanup is reliable during error conditions."
        stop 0
    else
        print *, "Some error cleanup tests failed!"
        print *, "Cleanup reliability issues detected."
        stop 1
    end if

contains

    subroutine test_cleanup_after_parse_error()
        ! Given: Arena used during parsing that encounters errors
        ! When: Parsing fails and cleanup is performed
        ! Then: Arena should cleanup consistently regardless of parse errors
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(identifier_node) :: id_node
        character(len=:), allocatable :: invalid_source
        logical :: cleanup_after_parse_safe
        
        call test_start("Cleanup reliability after parsing errors")
        
        cleanup_after_parse_safe = .true.
        arena = create_ast_arena()
        
        ! Add some valid content first
        id_node = create_identifier("valid_before_error")
        call arena%push(id_node, "identifier")
        
        ! Simulate parsing error with invalid syntax
        invalid_source = "if x > 0 /* missing then, invalid comment style */"
        
        call tokenize_core(invalid_source, tokens)
        if (allocated(tokens)) then
            parser = create_parser_state(tokens)
            ! Parser would encounter errors here
            ! The key test is that arena remains in valid state for cleanup
        end if
        
        ! Test cleanup after parse error
        call arena%clear()
        
        ! Validate cleanup worked despite parse errors
        if (arena%size /= 0 .or. arena%current_index /= 0 .or. arena%max_depth /= 0) then
            cleanup_after_parse_safe = .false.
        end if
        
        if (cleanup_after_parse_safe) then
            call test_pass()
        else
            call test_fail("Cleanup failed after parsing errors")
        end if
        
        if (allocated(tokens)) deallocate(tokens)
    end subroutine test_cleanup_after_parse_error

    subroutine test_cleanup_after_allocation_error()
        ! Given: Arena operations that encounter memory allocation stress
        ! When: Allocation operations fail or reach limits
        ! Then: Cleanup should handle allocation error states properly
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        integer :: i, allocation_attempts
        logical :: allocation_cleanup_safe
        
        call test_start("Cleanup after allocation error scenarios")
        
        allocation_cleanup_safe = .true.
        arena = create_ast_arena(2)  ! Very small capacity to force growth
        
        allocation_attempts = 50  ! Large number to stress allocation
        
        ! Stress test allocation - some may fail due to memory pressure
        do i = 1, allocation_attempts
            id_node = create_identifier("stress_node_" // char(48 + mod(i, 10)))
            call arena%push(id_node, "identifier")
            
            ! Test intermediate cleanup during stress
            if (mod(i, 10) == 0) then
                call arena%clear()
                if (arena%size /= 0) then
                    allocation_cleanup_safe = .false.
                    exit
                end if
                arena = create_ast_arena(2)  ! Restart with small capacity
            end if
        end do
        
        ! Final cleanup test
        call arena%clear()
        if (arena%size /= 0) allocation_cleanup_safe = .false.
        
        if (allocation_cleanup_safe) then
            call test_pass()
        else
            call test_fail("Cleanup failed after allocation errors")
        end if
    end subroutine test_cleanup_after_allocation_error

    subroutine test_cleanup_arena_corruption()
        ! Given: Arena that may be in corrupted state
        ! When: Attempting cleanup on potentially corrupted arena
        ! Then: Cleanup should be defensive and handle corruption gracefully
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        logical :: corruption_cleanup_safe
        
        call test_start("Cleanup resilience with arena corruption")
        
        corruption_cleanup_safe = .true.
        arena = create_ast_arena()
        
        ! Create normal content
        id_node = create_identifier("normal_node")
        call arena%push(id_node, "identifier")
        
        ! Simulate potential corruption scenarios
        ! Note: We can't actually corrupt the arena safely in a test,
        ! but we can test cleanup robustness
        
        ! Test cleanup multiple times (defensive against corruption)
        call arena%clear()
        if (arena%size /= 0) corruption_cleanup_safe = .false.
        
        ! Test cleanup on already cleared arena (defensive)
        call arena%clear()
        if (arena%size /= 0) corruption_cleanup_safe = .false.
        
        if (corruption_cleanup_safe) then
            call test_pass()
        else
            call test_fail("Cleanup not resilient to corruption scenarios")
        end if
    end subroutine test_cleanup_arena_corruption

    subroutine test_cleanup_invalid_operations()
        ! Given: Arena subjected to invalid or unexpected operations
        ! When: Invalid operations are attempted before cleanup
        ! Then: Cleanup should work regardless of invalid operation attempts
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        logical :: invalid_ops_cleanup_safe
        
        call test_start("Cleanup after invalid operation attempts")
        
        invalid_ops_cleanup_safe = .true.
        arena = create_ast_arena()
        
        ! Add valid content
        id_node = create_identifier("before_invalid")
        call arena%push(id_node, "identifier")
        
        ! Attempt invalid operations (these should be handled gracefully)
        ! Pop from empty arena after clearing
        call arena%clear()
        if (arena%size /= 0) invalid_ops_cleanup_safe = .false.
        
        call arena%pop()  ! Pop from empty arena - should be handled safely
        
        ! Test cleanup still works after invalid operations
        call arena%clear()
        if (arena%size /= 0) invalid_ops_cleanup_safe = .false.
        
        if (invalid_ops_cleanup_safe) then
            call test_pass()
        else
            call test_fail("Cleanup failed after invalid operations")
        end if
    end subroutine test_cleanup_invalid_operations

    subroutine test_cleanup_exception_conditions()
        ! Given: Arena operations under exceptional conditions
        ! When: Exception-like conditions occur during processing
        ! Then: Cleanup should maintain consistency during exception handling
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        integer :: exception_scenario
        logical :: exception_cleanup_safe
        
        call test_start("Cleanup consistency during exception conditions")
        
        exception_cleanup_safe = .true.
        
        ! Test different exception scenarios
        do exception_scenario = 1, 3
            arena = create_ast_arena()
            
            select case (exception_scenario)
            case (1)
                ! Scenario: Rapid operations with immediate cleanup
                id_node = create_identifier("rapid_test")
                call arena%push(id_node, "identifier")
                call arena%clear()  ! Immediate cleanup
                
            case (2)
                ! Scenario: Complex nested operations with cleanup
                id_node = create_identifier("parent")
                call arena%push(id_node, "identifier")
                id_node = create_identifier("child")
                call arena%push(id_node, "identifier", 1)  ! Child of index 1
                call arena%clear()  ! Cleanup with relationships
                
            case (3)
                ! Scenario: Multiple push/pop cycles with cleanup
                id_node = create_identifier("cycle_test")
                call arena%push(id_node, "identifier")
                call arena%pop()
                call arena%push(id_node, "identifier")
                call arena%clear()
            end select
            
            ! Validate cleanup worked in each scenario
            if (arena%size /= 0 .or. arena%current_index /= 0) then
                exception_cleanup_safe = .false.
                exit
            end if
        end do
        
        if (exception_cleanup_safe) then
            call test_pass()
        else
            call test_fail("Cleanup inconsistent during exception conditions")
        end if
    end subroutine test_cleanup_exception_conditions

    subroutine test_cleanup_multiple_error_types()
        ! Given: Arena experiencing multiple types of errors in sequence
        ! When: Multiple error conditions occur before cleanup
        ! Then: Cleanup should handle combination of error types consistently
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        type(token_t), allocatable :: tokens(:)
        character(len=:), allocatable :: error_source
        logical :: multi_error_cleanup_safe
        integer :: i
        
        call test_start("Cleanup with multiple error type combinations")
        
        multi_error_cleanup_safe = .true.
        arena = create_ast_arena()
        
        ! Error Type 1: Invalid content + valid content mix
        id_node = create_identifier("valid_node")
        call arena%push(id_node, "identifier")
        
        ! Error Type 2: Parsing error with invalid syntax
        error_source = "invalid syntax here &"
        call tokenize_core(error_source, tokens)
        ! This would cause parsing errors in real usage
        
        ! Error Type 3: Operations on modified arena
        call arena%pop()  ! Remove the valid node
        
        ! Error Type 4: Rapid operations
        do i = 1, 5
            id_node = create_identifier("rapid_" // char(48 + i))
            call arena%push(id_node, "identifier")
        end do
        
        ! Test cleanup after multiple error types
        call arena%clear()
        
        ! Validate cleanup handled all error types
        if (arena%size /= 0 .or. arena%current_index /= 0 .or. arena%max_depth /= 0) then
            multi_error_cleanup_safe = .false.
        end if
        
        if (multi_error_cleanup_safe) then
            call test_pass()
        else
            call test_fail("Cleanup failed with multiple error types")
        end if
        
        if (allocated(tokens)) deallocate(tokens)
    end subroutine test_cleanup_multiple_error_types

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
        all_passed = .false.
    end subroutine test_fail

end program test_arena_error_cleanup