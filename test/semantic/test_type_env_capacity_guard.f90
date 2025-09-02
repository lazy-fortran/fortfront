program test_type_env_capacity_guard
    use type_system_unified, only: type_env_t, mono_type_t, poly_type_t, &
        type_var_t, create_mono_type, create_poly_type, TINT
    implicit none

    type(type_env_t) :: env
    type(mono_type_t) :: m
    type(poly_type_t) :: p
    type(type_var_t), allocatable :: forall_vars(:)
    integer :: i

    ! Set a very small capacity to trigger guard logic quickly
    env%capacity = 2

    ! Create a simple type scheme to insert
    m = create_mono_type(TINT)
    allocate(forall_vars(0))
    p = create_poly_type(forall_vars, m)

    do i = 1, 10
        call env%extend('x', p)
    end do

    ! Expect the count to be capped at capacity (no overflow)
    if (env%count /= env%capacity) then
        print *, 'FAIL: env%count=', env%count, ' capacity=', env%capacity
        stop 1
    else
        print *, 'PASS: type_env capacity guard works (count=', env%count, ')'
    end if

end program test_type_env_capacity_guard
