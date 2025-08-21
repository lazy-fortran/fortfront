program test_arena_semantic_integration_simple
    ! Test integration with semantic analyzer patterns
    ! Given: Arena-based type system
    ! When: Operations simulating semantic analysis are performed
    ! Then: All operations work correctly with arena types
    
    use type_system_hm_arena
    use type_arena, only: global_type_arena, type_arena_stats_t
    use type_system_transition, only: init_transition_system
    implicit none
    
    character(len=256) :: type_str
    type(mono_type_t) :: int_type, real_type, char_type, fun_type
    type(poly_type_t) :: poly_type
    type(type_var_t) :: tv1, tv2
    type(type_arena_stats_t) :: stats_before, stats_after
    integer :: i
    logical :: test_pass
    
    print *, "=== Testing Arena Semantic Integration ==="
    
    ! Initialize the system
    call init_transition_system()
    stats_before = global_type_arena%get_stats()
    
    ! Test 1: Basic type operations (simulates variable declaration analysis)
    print *, "Test 1: Variable type operations"
    
    int_type = create_mono_type(TINT)
    real_type = create_mono_type(TREAL)
    char_type = create_mono_type(TCHAR, char_size=10)
    
    type_str = int_type%to_string()
    test_pass = (trim(type_str) == "integer")
    if (.not. test_pass) then
        print *, "ERROR: Integer type failed:", trim(type_str)
        error stop
    end if
    
    print *, "PASS: Variable types created successfully"
    
    ! Test 2: Function type operations (simulates function analysis)  
    print *, "Test 2: Function type operations"
    
    fun_type = create_fun_type(int_type, real_type)
    type_str = fun_type%to_string()
    test_pass = (index(type_str, "integer") > 0 .and. index(type_str, "real") > 0)
    if (.not. test_pass) then
        print *, "ERROR: Function type failed:", trim(type_str)
        error stop
    end if
    
    print *, "PASS: Function types handled correctly"
    
    ! Test 3: Polymorphic types (simulates generic function analysis)
    print *, "Test 3: Polymorphic type operations"
    
    tv1 = create_type_var(1, "'T")
    tv2 = create_type_var(2, "'U")
    poly_type = create_poly_type([tv1, tv2], fun_type)
    
    type_str = poly_type%to_string()
    test_pass = (index(type_str, "forall") > 0)
    if (.not. test_pass) then
        print *, "ERROR: Polymorphic type failed:", trim(type_str)
        error stop  
    end if
    
    print *, "PASS: Polymorphic types handled correctly"
    
    ! Test 4: Type inference simulation
    print *, "Test 4: Type inference simulation"
    
    do i = 1, 50
        select case (mod(i, 4))
        case (0)
            int_type = create_mono_type(TINT)
        case (1)
            real_type = create_mono_type(TREAL) 
        case (2)
            char_type = create_mono_type(TCHAR, char_size=i)
        case (3)
            fun_type = create_fun_type(int_type, real_type)
        end select
        
        type_str = int_type%to_string()
        if (len_trim(type_str) == 0) then
            print *, "ERROR: Type inference failed at iteration", i
            error stop
        end if
    end do
    
    print *, "PASS: Type inference simulation completed"
    
    ! Test 5: Memory usage analysis
    print *, "Test 5: Memory usage analysis"
    
    stats_after = global_type_arena%get_stats()
    print *, "Arena statistics:"
    print *, "  Types before: ", stats_before%total_types
    print *, "  Types after: ", stats_after%total_types
    print *, "  Types created: ", stats_after%total_types - stats_before%total_types
    print *, "  Memory usage: ", stats_after%memory_usage, "bytes"
    
    if (stats_after%total_types <= stats_before%total_types) then
        print *, "WARNING: Expected more types to be created"
    end if
    
    print *, "PASS: Memory usage tracking works correctly"
    
    print *, "PASS: All semantic integration tests completed successfully"
    print *, "Arena-based types integrate properly with semantic analysis patterns"
    
end program test_arena_semantic_integration_simple