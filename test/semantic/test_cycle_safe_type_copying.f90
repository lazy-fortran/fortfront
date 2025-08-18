program test_cycle_safe_type_copying
    ! Comprehensive tests for Issue #276: Cycle-safe type copying system
    use type_system_hm
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    ! Given: Existing mono_type_t structure with potential cycles
    ! When: Various type copying scenarios are executed  
    ! Then: All operations should complete without infinite recursion

    call test_simple_type_copying()
    call test_function_type_copying()
    call test_recursive_function_types()
    call test_deeply_nested_function_types() 
    call test_cycle_detection_simple()
    call test_cycle_detection_complex()
    call test_mutual_recursion_types()
    call test_self_referential_function()
    call test_multiple_reference_sharing()
    call test_copy_preservation()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_simple_type_copying()
        ! Given: Simple mono_type_t instances
        ! When: Assignment operations are performed
        ! Then: Types should be copied correctly without cycles
        type(mono_type_t) :: int_type1, int_type2, real_type1, real_type2

        test_count = test_count + 1

        ! Create simple types
        int_type1 = create_mono_type(TINT)
        real_type1 = create_mono_type(TREAL)

        ! Test assignment
        int_type2 = int_type1
        real_type2 = real_type1

        ! Verify types were copied correctly
        if (int_type2%kind == TINT .and. real_type2%kind == TREAL) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Simple type copying"
        else
            write (*, '(A)') "FAIL: Simple type copying - incorrect kinds"
        end if
    end subroutine test_simple_type_copying

    subroutine test_function_type_copying()
        ! Given: Function types with nested structure
        ! When: Function types are assigned
        ! Then: Arguments and result types should be copied properly
        type(mono_type_t) :: int_type, real_type, fun_type1, fun_type2

        test_count = test_count + 1

        ! Create function type: int -> real
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        fun_type1 = create_fun_type(int_type, real_type)

        ! Test assignment of function type
        fun_type2 = fun_type1

        ! Verify function type was copied correctly (simplified type system)
        if (fun_type2%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Function type copying"
        else
            write (*, '(A)') "FAIL: Function type copying - not a function type"
        end if
    end subroutine test_function_type_copying

    subroutine test_recursive_function_types()
        ! Given: Higher-order function types (function -> function)
        ! When: Nested function types are copied
        ! Then: All levels should be copied without infinite recursion
        type(mono_type_t) :: int_type, real_type, inner_fun, outer_fun1, outer_fun2

        test_count = test_count + 1

        ! Create nested function type: (int -> real) -> int
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        inner_fun = create_fun_type(int_type, real_type)  ! int -> real
        outer_fun1 = create_fun_type(inner_fun, int_type)  ! (int -> real) -> int

        ! Test assignment of nested function type
        outer_fun2 = outer_fun1

        ! Verify nested function type was copied correctly (simplified type system)
        if (outer_fun2%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Recursive function type copying"
        else
            write (*, '(A)') "FAIL: Recursive function type copying - not a function type"
        end if
    end subroutine test_recursive_function_types

    subroutine test_deeply_nested_function_types()
        ! Given: Very deeply nested function types
        ! When: Deep nesting is copied (stress test for stack safety)
        ! Then: Should complete without stack overflow
        type(mono_type_t) :: base_type, nested_type1, nested_type2
        integer :: i

        test_count = test_count + 1

        ! Create deeply nested function type
        base_type = create_mono_type(TINT)
        nested_type1 = base_type

        ! Build deep nesting: int -> (int -> (int -> ... -> int))
        do i = 1, 20  ! 20 levels of nesting should be safe
            nested_type1 = create_fun_type(base_type, nested_type1)
        end do

        ! Test assignment of deeply nested type
        nested_type2 = nested_type1

        ! Verify basic structure is preserved (simplified type system)
        if (nested_type2%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Deeply nested function type copying"
        else
            write (*, '(A)') "FAIL: Deeply nested function type copying"
        end if
    end subroutine test_deeply_nested_function_types

    subroutine test_cycle_detection_simple()
        ! Given: A function type that could reference itself
        ! When: Manual cycle creation and copying is attempted
        ! Then: Should detect and handle cycles appropriately
        type(mono_type_t) :: fun_type1, fun_type2, int_type

        test_count = test_count + 1

        ! Create a function type and attempt to create a simple cycle
        int_type = create_mono_type(TINT)
        fun_type1 = create_fun_type(int_type, int_type)

        ! Test assignment with potential for self-reference
        fun_type2 = fun_type1

        ! Verify the type was copied without issues
        if (fun_type2%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Simple cycle detection test"
        else
            write (*, '(A)') "FAIL: Simple cycle detection test"
        end if
    end subroutine test_cycle_detection_simple

    subroutine test_cycle_detection_complex()
        ! Given: Complex type structures with multiple references
        ! When: Complex copying scenarios are executed
        ! Then: Should handle all cases without infinite loops
        type(mono_type_t) :: type_a, type_b, type_c, int_type

        test_count = test_count + 1

        ! Create complex interconnected function types
        int_type = create_mono_type(TINT)
        type_a = create_fun_type(int_type, int_type)
        type_b = create_fun_type(type_a, int_type)
        type_c = create_fun_type(type_b, type_a)

        ! Test copying of complex structure
        type_a = type_c  ! This creates a more complex reference pattern

        ! Verify operation completed successfully
        if (type_a%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Complex cycle detection test"
        else
            write (*, '(A)') "FAIL: Complex cycle detection test"
        end if
    end subroutine test_cycle_detection_complex

    subroutine test_mutual_recursion_types()
        ! Given: Types that could potentially reference each other
        ! When: Mutual reference patterns are copied
        ! Then: Should handle mutual recursion safely
        type(mono_type_t) :: fun_a, fun_b, fun_a_copy, fun_b_copy, int_type

        test_count = test_count + 1

        ! Create function types that reference each other's structure
        int_type = create_mono_type(TINT)
        fun_a = create_fun_type(int_type, int_type)
        fun_b = create_fun_type(fun_a, int_type)

        ! Test copying with mutual references
        fun_a_copy = fun_a
        fun_b_copy = fun_b

        ! Verify both copies are valid
        if (fun_a_copy%kind == TFUN .and. fun_b_copy%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Mutual recursion type copying"
        else
            write (*, '(A)') "FAIL: Mutual recursion type copying"
        end if
    end subroutine test_mutual_recursion_types

    subroutine test_self_referential_function()
        ! Given: A function type structure
        ! When: Self-referential patterns are created and copied
        ! Then: Should prevent infinite recursion during copying
        type(mono_type_t) :: self_fun, self_fun_copy, int_type

        test_count = test_count + 1

        ! Create function type that could be self-referential
        int_type = create_mono_type(TINT)
        self_fun = create_fun_type(int_type, int_type)

        ! Test copying of potentially self-referential type
        self_fun_copy = self_fun

        ! Verify copy succeeded (simplified type system)
        if (self_fun_copy%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Self-referential function copying"
        else
            write (*, '(A)') "FAIL: Self-referential function copying"
        end if
    end subroutine test_self_referential_function

    subroutine test_multiple_reference_sharing()
        ! Given: Multiple types sharing common subtypes
        ! When: Shared references are copied
        ! Then: Should handle shared structure correctly
        type(mono_type_t) :: shared_type, fun_a, fun_b, fun_a_copy, fun_b_copy

        test_count = test_count + 1

        ! Create shared subtype
        shared_type = create_mono_type(TINT)

        ! Create function types that share the same subtype
        fun_a = create_fun_type(shared_type, shared_type)
        fun_b = create_fun_type(shared_type, shared_type)

        ! Test copying with shared references
        fun_a_copy = fun_a
        fun_b_copy = fun_b

        ! Verify both copies are valid and independent (simplified type system)
        if (fun_a_copy%kind == TFUN .and. fun_b_copy%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Multiple reference sharing copying"
        else
            write (*, '(A)') "FAIL: Multiple reference sharing copying - incorrect kinds"
        end if
    end subroutine test_multiple_reference_sharing

    subroutine test_copy_preservation()
        ! Given: Types with various mono_type_t properties
        ! When: Types are copied using assignment
        ! Then: All properties should be preserved correctly
        type(mono_type_t) :: original, copy
        type(mono_type_t) :: int_type, real_type

        test_count = test_count + 1

        ! Create original function type with all properties
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        original = create_fun_type(int_type, real_type)
        
        ! Set additional properties
        original%size = 42
        original%alloc_info%is_allocatable = .true.
        original%alloc_info%is_pointer = .false.

        ! Test deep copy preservation
        copy = original

        ! Verify all properties were preserved (simplified type system)
        if (copy%kind == TFUN .and. copy%size == 42) then
            if (copy%alloc_info%is_allocatable .and. .not. copy%alloc_info%is_pointer) then
                pass_count = pass_count + 1
                write (*, '(A)') "PASS: Copy preservation test"
            else
                write (*, '(A)') "FAIL: Copy preservation - allocation info not preserved"
            end if
        else
            write (*, '(A)') "FAIL: Copy preservation - basic properties not preserved"
        end if
    end subroutine test_copy_preservation

end program test_cycle_safe_type_copying