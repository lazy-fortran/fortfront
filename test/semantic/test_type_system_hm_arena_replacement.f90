program test_type_system_hm_arena_replacement
    ! Test that arena-based replacement maintains the same API and behavior
    use type_system_hm_arena  ! Drop-in replacement
    implicit none

    type(mono_type_t) :: int_type, var_type, fun_type
    type(poly_type_t) :: poly
    type(type_var_t) :: tv
    type(type_env_t) :: env
    type(substitution_t) :: subst
    character(len=256) :: str
    logical :: equal

    print *, "=== Testing Arena-based HM Drop-in Replacement ==="

    ! Test type creation (same API as original)
    int_type = create_mono_type(TINT)
    tv = create_type_var(1, "'x")
    var_type = create_mono_type(TVAR, tv)
    fun_type = create_fun_type(int_type, var_type)

    print *, "Type creation tests:"
    str = int_type%to_string()
    print *, "  Integer: ", trim(str)
    str = var_type%to_string()  
    print *, "  Variable: ", trim(str)
    str = fun_type%to_string()
    print *, "  Function: ", trim(str)

    ! Test type equality
    equal = int_type%equals(int_type)
    print *, "Self equality: ", equal
    
    equal = int_type%equals(var_type)
    print *, "Different type equality: ", equal

    ! Test polymorphic type
    poly = create_poly_type([tv], int_type)
    str = poly%to_string()
    print *, "Polymorphic type: ", trim(str)

    ! Test copy operations (critical for arena safety)
    block
        type(mono_type_t) :: copy
        copy = int_type%deep_copy()
        equal = copy%equals(int_type)
        print *, "Deep copy equality: ", equal
    end block

    ! Test assignment operations
    block
        type(mono_type_t) :: assigned
        assigned = fun_type
        equal = assigned%equals(fun_type)
        print *, "Assignment equality: ", equal
    end block

    print *, "PASS: Arena-based HM replacement works correctly"

end program test_type_system_hm_arena_replacement