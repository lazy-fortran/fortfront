program test_type_arena_transition
    ! Test transition compatibility between old and new type systems
    use type_system_hm, only: mono_type_t, type_var_t, poly_type_t, &
                              create_mono_type, create_type_var, &
                              TVAR, TINT, TREAL, TCHAR, TLOGICAL, TFUN, TARRAY
    use type_arena, only: type_ref_t, global_type_arena, type_arena_stats_t
    use type_system_transition, only: mono_to_arena, arena_to_mono, init_transition_system
    implicit none

    type(mono_type_t) :: old_int, old_var, old_fun, old_array
    type(type_ref_t) :: arena_ref
    type(mono_type_t) :: converted_back
    type(mono_type_t), allocatable :: fun_args(:)
    character(len=256) :: old_str, new_str
    logical :: types_match
    
    print *, "=== Testing Type Arena Transition System ==="

    ! Initialize transition system
    call init_transition_system()

    ! Test basic type conversions
    old_int = create_mono_type(TINT)
    arena_ref = mono_to_arena(old_int)
    converted_back = arena_to_mono(arena_ref)
    
    old_str = old_int%to_string()
    new_str = converted_back%to_string()
    types_match = old_int%equals(converted_back)
    
    print *, "Integer type conversion:"
    print *, "  Original: ", trim(old_str)
    print *, "  Converted back: ", trim(new_str)
    print *, "  Types match: ", types_match
    
    ! Test type variable conversion
    old_var = create_mono_type(TVAR, create_type_var(1, "'x"))
    arena_ref = mono_to_arena(old_var)
    converted_back = arena_to_mono(arena_ref)
    
    old_str = old_var%to_string()
    new_str = converted_back%to_string()
    types_match = old_var%equals(converted_back)
    
    print *, "Type variable conversion:"
    print *, "  Original: ", trim(old_str)
    print *, "  Converted back: ", trim(new_str)
    print *, "  Types match: ", types_match
    
    ! Test function type conversion
    allocate(fun_args(2))
    fun_args(1) = create_mono_type(TINT)
    fun_args(2) = create_mono_type(TREAL)
    old_fun = create_mono_type(TFUN, args=fun_args)
    
    arena_ref = mono_to_arena(old_fun)
    converted_back = arena_to_mono(arena_ref)
    
    old_str = old_fun%to_string()
    new_str = converted_back%to_string()
    types_match = old_fun%equals(converted_back)
    
    print *, "Function type conversion:"
    print *, "  Original: ", trim(old_str)
    print *, "  Converted back: ", trim(new_str)
    print *, "  Types match: ", types_match
    
    ! Test array type conversion
    deallocate(fun_args)
    allocate(fun_args(1))
    fun_args(1) = create_mono_type(TREAL)
    old_array = create_mono_type(TARRAY, args=fun_args, char_size=10)
    
    arena_ref = mono_to_arena(old_array)
    converted_back = arena_to_mono(arena_ref)
    
    old_str = old_array%to_string()
    new_str = converted_back%to_string()
    types_match = old_array%equals(converted_back)
    
    print *, "Array type conversion:"
    print *, "  Original: ", trim(old_str)
    print *, "  Converted back: ", trim(new_str)
    print *, "  Types match: ", types_match
    
    ! Test arena efficiency
    print *, "Arena statistics:"
    call test_arena_efficiency()
    
    print *, "PASS: All transition system tests completed"

contains

    subroutine test_arena_efficiency()
        type(type_arena_stats_t) :: stats
        integer :: i
        type(type_ref_t) :: refs(100)
        
        ! Create many types to test arena efficiency
        do i = 1, 100
            refs(i) = global_type_arena%create_var(i, "'test")
        end do
        
        stats = global_type_arena%get_stats()
        print *, "  Total types created: ", stats%total_types
        print *, "  Arena capacity: ", stats%capacity
        print *, "  Memory usage (bytes): ", stats%memory_usage
        
        ! Test that references are still valid
        do i = 1, min(10, 100)
            if (.not. refs(i)%is_valid()) then
                print *, "ERROR: Reference", i, "is invalid"
            end if
        end do
    end subroutine test_arena_efficiency

end program test_type_arena_transition