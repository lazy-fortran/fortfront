program test_type_arena_basic
    ! Basic test for type arena system
    use type_arena
    use type_env_arena
    use type_subst_arena
    use type_system_arena
    implicit none

    type(type_ref_t) :: int_ref, real_ref, char_ref, var_ref, fun_ref, array_ref
    type(type_env_arena_t) :: env
    type(type_subst_arena_t) :: subst
    type(mono_type_arena_t) :: mono_int, mono_var
    character(len=256) :: str
    logical :: equal

    print *, "=== Testing Type Arena Basic Functionality ==="

    ! Initialize global arena
    call init_type_arena(global_type_arena)

    ! Test basic type creation
    int_ref = global_type_arena%create_int()
    real_ref = global_type_arena%create_real()
    char_ref = global_type_arena%create_char(10)
    var_ref = global_type_arena%create_var(1, "'a")

    print *, "Created basic types:"
    str = int_ref%to_string()
    print *, "  Integer: ", trim(str)
    str = real_ref%to_string()
    print *, "  Real: ", trim(str)
    str = char_ref%to_string()
    print *, "  Character: ", trim(str)
    str = var_ref%to_string()
    print *, "  Variable: ", trim(str)

    ! Test function type creation
    fun_ref = global_type_arena%create_fun([int_ref], real_ref)
    str = fun_ref%to_string()
    print *, "  Function: ", trim(str)

    ! Test array type creation
    array_ref = global_type_arena%create_array(int_ref, 5)
    str = array_ref%to_string()
    print *, "  Array: ", trim(str)

    ! Test type equality
    equal = int_ref%equals(int_ref)
    print *, "Self equality test: ", equal

    equal = int_ref%equals(real_ref)
    print *, "Different type equality test: ", equal

    ! Test type environment
    env = create_type_env_arena()
    call env%extend("x", int_ref)
    call env%extend("y", real_ref)

    print *, "Environment lookup test:"
    int_ref = env%lookup("x")
    str = int_ref%to_string()
    print *, "  x: ", trim(str)

    real_ref = env%lookup("y")
    str = real_ref%to_string()
    print *, "  y: ", trim(str)

    ! Test substitution
    subst = create_type_subst_arena()
    call subst%add(1, int_ref)  ! 'a -> integer

    var_ref = global_type_arena%create_var(1, "'a")
    int_ref = subst%apply(var_ref)
    str = int_ref%to_string()
    print *, "Substitution test ('a -> integer): ", trim(str)

    ! Test compatibility layer
    mono_int = create_mono_type_arena(TINT)
    mono_var = create_mono_type_arena(TVAR, create_type_var_arena(1))

    str = mono_int%to_string()
    print *, "Compatibility mono type (int): ", trim(str)
    str = mono_var%to_string()
    print *, "Compatibility mono type (var): ", trim(str)

    equal = mono_int%equals(mono_int)
    print *, "Compatibility equality test: ", equal

    print *, "PASS: All basic arena tests completed successfully"

end program test_type_arena_basic