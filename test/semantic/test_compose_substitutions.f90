program test_compose_substitutions
    use type_system_unified, only: substitution_t, compose_substitutions, &
        type_var_t, mono_type_t, &
        create_type_var, create_mono_type, TINT
    implicit none

    type(type_var_t) :: a, b
    type(mono_type_t) :: int_type, applied
    type(substitution_t) :: s1, s2, composed
    logical :: ok

    ok = .true.

    int_type = create_mono_type(TINT)
    a = create_type_var(1, "a")
    b = create_type_var(2, "b")

    ! s2 maps b -> a (as a mono_type wrapping the type var)
    call s2%add(b, var_to_mono(a))

    ! s1 maps a -> int
    call s1%add(a, int_type)

    ! Composing s1 . s2 should give b -> int and a -> int
    composed = compose_substitutions(s1, s2)

    if (composed%count /= 2) then
        print *, "FAIL: composed has ", composed%count, " bindings, expected 2"
        ok = .false.
    end if

    call composed%apply(var_to_mono(b), applied)
    if (applied%kind /= TINT) then
        print *, "FAIL: composed(b) kind=", applied%kind, " expected TINT=", TINT
        ok = .false.
    else
        print *, "PASS: composed(b) -> int"
    end if

    call composed%apply(var_to_mono(a), applied)
    if (applied%kind /= TINT) then
        print *, "FAIL: composed(a) kind=", applied%kind, " expected TINT=", TINT
        ok = .false.
    else
        print *, "PASS: composed(a) -> int"
    end if

    if (.not. ok) error stop 1
    print *, "=== compose_substitutions tests passed ==="
contains
    function var_to_mono(v) result(mt)
        type(type_var_t), intent(in) :: v
        type(mono_type_t) :: mt
        mt = create_mono_type(1, var=v) ! TVAR = 1
    end function var_to_mono
end program test_compose_substitutions
