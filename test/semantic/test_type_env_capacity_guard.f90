program test_type_env_capacity_guard
    use type_system_unified, only: type_env_t, mono_type_t, poly_type_t, &
        type_var_t, create_mono_type, create_poly_type, TINT
    use identifier_table, only: identifier_table_t, identifier_table_init
    implicit none

    type(type_env_t) :: env
    type(mono_type_t) :: m
    type(poly_type_t) :: p
    type(type_var_t), allocatable :: forall_vars(:)
    type(identifier_table_t), target :: table

    ! Set a very small capacity to trigger guard logic quickly
    env%capacity = 2
    env%is_fixed = .true.
    call identifier_table_init(table)
    env%identifiers => table

    ! Create a simple type scheme to insert
    m = create_mono_type(TINT)
    allocate (forall_vars(0))
    p = create_poly_type(forall_vars, m)

    call env%extend('x1', p)
    call env%extend('x2', p)
    call env%extend('x3', p)

    if (env%count /= env%capacity) then
        print *, 'FAIL: guard did not enforce fixed capacity'
        print *, 'count=', env%count, 'capacity=', env%capacity
        stop 1
    end if

    call env%extend('x2', p)
    if (env%count /= env%capacity) then
        print *, 'FAIL: duplicate definition altered count'
        stop 1
    end if

    print *, 'PASS: type_env capacity guard respected fixed capacity'

end program test_type_env_capacity_guard
