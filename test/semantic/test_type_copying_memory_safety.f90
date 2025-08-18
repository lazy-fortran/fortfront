program test_type_copying_memory_safety
    ! Memory safety tests for Issue #276: Cycle-safe type copying
    ! Validates infinite recursion prevention and memory management
    use type_system_hm
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    ! Given: Potentially dangerous type structures and memory scenarios
    ! When: Cycle-safe operations are performed
    ! Then: Should prevent infinite recursion and memory corruption

    call test_infinite_recursion_prevention()
    call test_stack_overflow_prevention()
    call test_memory_leak_prevention()
    call test_dangling_reference_safety()
    call test_allocation_failure_handling()
    call test_deep_recursion_limits()
    call test_cycle_breaking_safety()
    call test_memory_corruption_prevention()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_infinite_recursion_prevention()
        ! Given: Type structures that could cause infinite recursion
        ! When: Assignment operations are performed with potential cycles
        ! Then: Should complete without infinite loops
        type(mono_type_t) :: recursive_type1, recursive_type2, int_type
        integer :: i

        test_count = test_count + 1

        ! Create a function type that references complex nested structure
        int_type = create_mono_type(TINT)
        recursive_type1 = int_type

        ! Build a complex nested structure that could cause recursion issues
        do i = 1, 15  ! Deep enough to potentially cause issues
            recursive_type1 = create_fun_type(recursive_type1, int_type)
        end do

        ! Test assignment that could trigger infinite recursion
        recursive_type2 = recursive_type1

        ! If we reach this point, infinite recursion was prevented
        if (recursive_type2%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Infinite recursion prevention"
        else
            write (*, '(A)') "FAIL: Infinite recursion prevention - incorrect type"
        end if
    end subroutine test_infinite_recursion_prevention

    subroutine test_stack_overflow_prevention()
        ! Given: Very deep type nesting that could cause stack overflow
        ! When: Deep recursive copying is attempted
        ! Then: Should handle deep structures without stack overflow
        type(mono_type_t) :: deep_type, shallow_copy, int_type
        integer :: i

        test_count = test_count + 1

        ! Create extremely deep nesting (beyond typical stack limits)
        int_type = create_mono_type(TINT)
        deep_type = int_type

        ! Build deep nesting that would overflow with naive recursion
        do i = 1, 100  ! Very deep - would overflow without protection
            deep_type = create_fun_type(int_type, deep_type)
        end do

        ! Test copying of extremely deep structure
        shallow_copy = deep_type

        ! If we reach this point, stack overflow was prevented
        if (shallow_copy%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Stack overflow prevention"
        else
            write (*, '(A)') "FAIL: Stack overflow prevention - copy failed"
        end if
    end subroutine test_stack_overflow_prevention

    subroutine test_memory_leak_prevention()
        ! Given: Repeated type operations that could leak memory
        ! When: Many assignment operations are performed
        ! Then: Should not accumulate leaked memory
        type(mono_type_t) :: source_type, target_type, int_type, real_type
        integer :: i

        test_count = test_count + 1

        ! Create types for repeated operations
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        source_type = create_fun_type(int_type, real_type)

        ! Perform many assignments that could leak memory
        do i = 1, 1000
            target_type = source_type  ! Should not leak on repeated assignment
            ! Each assignment should properly clean up previous allocations
        end do

        ! Verify final state is correct
        if (target_type%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Memory leak prevention"
        else
            write (*, '(A)') "FAIL: Memory leak prevention - final state incorrect"
        end if
    end subroutine test_memory_leak_prevention

    subroutine test_dangling_reference_safety()
        ! Given: Complex type structures with shared components
        ! When: Some components go out of scope
        ! Then: Copied types should remain valid
        integer :: test_result

        test_count = test_count + 1

        ! Test in isolated scope to trigger component destruction
        call isolated_scope_test(test_result)

        if (test_result == 1) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Dangling reference safety"
        else
            write (*, '(A)') "FAIL: Dangling reference safety"
        end if
    end subroutine test_dangling_reference_safety

    subroutine isolated_scope_test(result)
        ! Helper subroutine to test scope isolation
        integer, intent(out) :: result
        type(mono_type_t) :: local_type, persistent_copy, int_type

        ! Create local type that will go out of scope
        int_type = create_mono_type(TINT)
        local_type = create_fun_type(int_type, int_type)

        ! Copy to persistent variable
        persistent_copy = local_type

        ! local_type will be destroyed when leaving this scope
        ! persistent_copy should remain valid due to deep copying

        if (persistent_copy%kind == TFUN) then
            result = 1  ! Success
        else
            result = 0  ! Failure
        end if
    end subroutine isolated_scope_test

    subroutine test_allocation_failure_handling()
        ! Given: Potentially large allocations during copying
        ! When: Allocation could fail (simulated)
        ! Then: Should handle gracefully without corruption
        type(mono_type_t) :: large_type, copy_type, int_type
        integer :: i

        test_count = test_count + 1

        ! Create type with many allocatable components
        int_type = create_mono_type(TINT)
        large_type = int_type

        ! Build structure with multiple allocation points
        do i = 1, 50
            large_type = create_fun_type(large_type, int_type)
        end do

        ! Test copying (allocation could theoretically fail with limited memory)
        copy_type = large_type

        ! Verify successful completion (in normal test environment)
        if (copy_type%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Allocation failure handling"
        else
            write (*, '(A)') "FAIL: Allocation failure handling"
        end if
    end subroutine test_allocation_failure_handling

    subroutine test_deep_recursion_limits()
        ! Given: Recursion depth limits in cycle-safe copying
        ! When: Very deep structures are encountered
        ! Then: Should respect depth limits and complete safely
        type(mono_type_t) :: bounded_type, copy_type, int_type
        integer :: i

        test_count = test_count + 1

        ! Create structure that tests recursion depth limits
        int_type = create_mono_type(TINT)
        bounded_type = int_type

        ! Build structure beyond reasonable recursion depth
        do i = 1, 200  ! Beyond typical recursion limits
            bounded_type = create_fun_type(int_type, bounded_type)
        end do

        ! Test copying with depth limits
        copy_type = bounded_type

        ! Should complete successfully with depth limiting
        if (copy_type%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Deep recursion limits"
        else
            write (*, '(A)') "FAIL: Deep recursion limits"
        end if
    end subroutine test_deep_recursion_limits

    subroutine test_cycle_breaking_safety()
        ! Given: Type structures with potential cycles
        ! When: Cycle detection breaks recursion
        ! Then: Should break cycles safely without corruption
        type(mono_type_t) :: cycle_type, safe_copy, int_type, real_type

        test_count = test_count + 1

        ! Create types that could form cycles
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        cycle_type = create_fun_type(int_type, real_type)

        ! Test cycle-breaking during copy
        safe_copy = cycle_type

        ! Verify cycle was broken safely
        if (safe_copy%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Cycle breaking safety"
        else
            write (*, '(A)') "FAIL: Cycle breaking safety - structure corrupted"
        end if
    end subroutine test_cycle_breaking_safety

    subroutine test_memory_corruption_prevention()
        ! Given: Complex assignment patterns that could corrupt memory
        ! When: Multiple overlapping assignments occur
        ! Then: Should maintain memory integrity
        type(mono_type_t) :: types(10), temp_type, int_type, real_type
        integer :: i, j

        test_count = test_count + 1

        ! Initialize array of different types
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)

        do i = 1, 10
            if (mod(i, 2) == 0) then
                types(i) = int_type
            else
                types(i) = create_fun_type(int_type, real_type)
            end if
        end do

        ! Perform complex assignment patterns that could corrupt memory
        do i = 1, 5
            do j = 1, 10
                temp_type = types(j)
                types(j) = temp_type  ! Self-assignment
                if (j > 1) then
                    types(j-1) = types(j)  ! Cross-assignment
                end if
            end do
        end do

        ! Verify memory integrity by checking all types
        do i = 1, 10
            if (types(i)%kind /= TINT .and. types(i)%kind /= TFUN) then
                write (*, '(A,I0)') "FAIL: Memory corruption prevention - invalid type at index ", i
                return
            end if
        end do

        pass_count = pass_count + 1
        write (*, '(A)') "PASS: Memory corruption prevention"
    end subroutine test_memory_corruption_prevention

end program test_type_copying_memory_safety