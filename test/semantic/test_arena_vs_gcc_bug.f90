program test_arena_vs_gcc_bug
    ! Test demonstrating type system arena eliminates GCC Bug 114612
    ! Shows safe function types without self-referential allocatable components
    
    use type_system_arena
    use type_system_unified, only: TVAR, TINT, TREAL, TFUN
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Arena vs GCC Bug 114612 Test ==="
    
    ! Test complex function type creation that would crash with old allocatable approach
    call test_function_type_creation()
    call test_nested_function_types()
    call test_recursive_type_structures()
    call test_bulk_type_operations()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All GCC bug elimination tests passed!"
        print *, "Type system arena successfully eliminates GCC Bug 114612!"
        stop 0
    else
        print *, "Some tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_function_type_creation()
        type(type_arena_t) :: arena
        type(arena_mono_type_t) :: int_type, real_type, func_type
        type(mono_handle_t) :: int_handle, real_handle, func_handle
        type(mono_handle_t), allocatable :: args(:)
        type(args_handle_t) :: args_handle
        
        call test_start("Complex function type creation")
        
        arena = create_type_arena()
        
        ! Create basic types
        int_type%kind = TINT
        int_type%var_id = 1
        int_type%var_name = "integer"
        int_handle = store_mono_type(arena, int_type)
        
        real_type%kind = TREAL
        real_type%var_id = 2
        real_type%var_name = "real"
        real_handle = store_mono_type(arena, real_type)
        
        ! Create function type: int -> real
        allocate(args(2))
        args(1) = int_handle
        args(2) = real_handle
        args_handle = store_type_args(arena, args)
        
        func_type%kind = TFUN
        func_type%var_id = 3
        func_type%var_name = "func_int_to_real"
        func_type%args = args_handle
        func_handle = store_mono_type(arena, func_type)
        
        ! Verify the function type was created successfully
        if (is_valid_mono_handle(func_handle) .and. &
            is_valid_args_handle(args_handle) .and. &
            args_handle%count == 2) then
            call test_pass()
        else
            call test_fail("Function type creation failed")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_function_type_creation
    
    subroutine test_nested_function_types()
        type(type_arena_t) :: arena
        type(arena_mono_type_t) :: int_type, func1_type, func2_type
        type(mono_handle_t) :: int_handle, func1_handle, func2_handle
        type(mono_handle_t), allocatable :: args1(:), args2(:)
        type(args_handle_t) :: args1_handle, args2_handle
        
        call test_start("Nested function types (function returning function)")
        
        arena = create_type_arena()
        
        ! Create int type
        int_type%kind = TINT
        int_type%var_id = 1
        int_type%var_name = "integer"
        int_handle = store_mono_type(arena, int_type)
        
        ! Create first function: int -> int
        allocate(args1(2))
        args1(1) = int_handle
        args1(2) = int_handle
        args1_handle = store_type_args(arena, args1)
        
        func1_type%kind = TFUN
        func1_type%var_id = 2
        func1_type%var_name = "int_to_int"
        func1_type%args = args1_handle
        func1_handle = store_mono_type(arena, func1_type)
        
        ! Create second function: int -> (int -> int)
        allocate(args2(2))
        args2(1) = int_handle
        args2(2) = func1_handle
        args2_handle = store_type_args(arena, args2)
        
        func2_type%kind = TFUN
        func2_type%var_id = 3
        func2_type%var_name = "curried_function"
        func2_type%args = args2_handle
        func2_handle = store_mono_type(arena, func2_type)
        
        ! Verify nested function type creation
        if (is_valid_mono_handle(func2_handle) .and. &
            is_valid_args_handle(args2_handle) .and. &
            args2_handle%count == 2) then
            call test_pass()
        else
            call test_fail("Nested function type creation failed")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_nested_function_types
    
    subroutine test_recursive_type_structures()
        type(type_arena_t) :: arena
        type(arena_mono_type_t) :: var_type, func_type
        type(mono_handle_t) :: var_handle, func_handle
        type(mono_handle_t), allocatable :: args(:)
        type(args_handle_t) :: args_handle
        type(type_arena_stats_t) :: stats_before, stats_after
        
        call test_start("Recursive type structures with type variables")
        
        arena = create_type_arena()
        stats_before = arena%get_stats()
        
        ! Create type variable 'a
        var_type%kind = TVAR
        var_type%var_id = 1
        var_type%var_name = "'a"
        var_handle = store_mono_type(arena, var_type)
        
        ! Create recursive function: 'a -> 'a
        allocate(args(2))
        args(1) = var_handle
        args(2) = var_handle
        args_handle = store_type_args(arena, args)
        
        func_type%kind = TFUN
        func_type%var_id = 2
        func_type%var_name = "identity"
        func_type%args = args_handle
        func_handle = store_mono_type(arena, func_type)
        
        stats_after = arena%get_stats()
        
        ! Verify recursive structure creation and memory efficiency
        if (is_valid_mono_handle(func_handle) .and. &
            stats_after%mono_types == 2 .and. &
            stats_after%arg_arrays == 1) then
            call test_pass()
        else
            call test_fail("Recursive type structure creation failed")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_recursive_type_structures
    
    subroutine test_bulk_type_operations()
        type(type_arena_t) :: arena
        type(arena_mono_type_t) :: base_type, func_type
        type(mono_handle_t) :: handles(100), func_handles(50)
        type(mono_handle_t), allocatable :: args(:)
        type(args_handle_t) :: args_handles(50)
        type(type_arena_stats_t) :: stats
        integer :: i
        
        call test_start("Bulk type operations performance")
        
        arena = create_type_arena()
        
        ! Create 100 base types
        base_type%kind = TINT
        do i = 1, 100
            base_type%var_id = i
            write(base_type%var_name, '(A,I0)') "type", i
            handles(i) = store_mono_type(arena, base_type)
        end do
        
        ! Create 50 function types using pairs of base types
        allocate(args(2))
        do i = 1, 50
            args(1) = handles(i)
            args(2) = handles(i + 50)
            args_handles(i) = store_type_args(arena, args)
            
            func_type%kind = TFUN
            func_type%var_id = 100 + i
            write(func_type%var_name, '(A,I0)') "func", i
            func_type%args = args_handles(i)
            func_handles(i) = store_mono_type(arena, func_type)
        end do
        
        ! Verify all operations succeeded
        stats = arena%get_stats()
        if (stats%mono_types == 150 .and. &
            stats%arg_arrays == 50 .and. &
            all([(is_valid_mono_handle(handles(i)), i=1,100)]) .and. &
            all([(is_valid_mono_handle(func_handles(i)), i=1,50)])) then
            call test_pass()
        else
            call test_fail("Bulk operations failed")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_bulk_type_operations
    
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
    
end program test_arena_vs_gcc_bug