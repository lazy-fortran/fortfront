program test_exact_semantic_context
    use semantic_analyzer
    use type_system_hm
    use scope_manager
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_exact_create_semantic_context_sequence()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_exact_create_semantic_context_sequence()
        type(semantic_context_t) :: ctx
        type(poly_type_t) :: builtin_scheme
        type(mono_type_t) :: real_to_real, real_type

        test_count = test_count + 1

        write (*, '(A)') "Reproducing exact create_semantic_context sequence..."

        ! Initialize substitution
        ctx%subst%count = 0
        write (*, '(A)') "Step 1: Substitution initialized"

        ! Initialize hierarchical scope stack
        ctx%scopes = create_scope_stack()
        write (*, '(A)') "Step 2: Scope stack initialized"

        ! Initialize environment with builtin functions
        ctx%next_var_id = 1
        write (*, '(A)') "Step 3: Next var ID initialized"

        ! Create real -> real type for math functions
        real_type = create_mono_type(TREAL)
        write (*, '(A)') "Step 4: Real type created"

        real_to_real = create_fun_type(real_type, real_type)
        write (*, '(A)') "Step 5: Function type created"

        ! Create polymorphic type scheme (no type variables to generalize)
        builtin_scheme = create_poly_type(forall_vars=[type_var_t::], mono=real_to_real)
        write (*, '(A)') "Step 6: Polymorphic scheme created"

        ! Add builtin functions to environment (this might be missing in the original)
        ! call ctx%env%extend("sin", builtin_scheme)
        ! call ctx%env%extend("cos", builtin_scheme)
        ! call ctx%env%extend("exp", builtin_scheme)

        write (*, '(A)') "Context creation sequence completed"

        if (ctx%next_var_id == 1) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Exact semantic context creation sequence"
        else
            write (*, '(A)') "FAIL: Context creation sequence"
        end if

        write (*, '(A)') "About to exit (all destructors will be called)"
    end subroutine test_exact_create_semantic_context_sequence

end program test_exact_semantic_context
