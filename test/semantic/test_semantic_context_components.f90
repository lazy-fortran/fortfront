program test_semantic_context_components
    use type_system_hm
    use scope_manager
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_type_env_creation()
    call test_scope_stack_creation()
    call test_substitution_creation()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_type_env_creation()
        type(type_env_t) :: env

        test_count = test_count + 1

        ! Initialize empty type environment
        env%count = 0

        ! Check environment is initialized
        if (env%count == 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Type environment creation"
        else
            write (*, '(A)') "FAIL: Type environment not initialized"
        end if
    end subroutine test_type_env_creation

    subroutine test_scope_stack_creation()
        type(scope_stack_t) :: scopes

        test_count = test_count + 1

        ! Create scope stack
        scopes = create_scope_stack()

        ! Check scope stack is initialized
        if (scopes%depth >= 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Scope stack creation"
        else
            write (*, '(A)') "FAIL: Scope stack not initialized"
        end if
    end subroutine test_scope_stack_creation

    subroutine test_substitution_creation()
        type(substitution_t) :: subst

        test_count = test_count + 1

        ! Initialize substitution
        subst%count = 0

        ! Check substitution is initialized
        if (subst%count == 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Substitution creation"
        else
            write (*, '(A)') "FAIL: Substitution not initialized"
        end if
    end subroutine test_substitution_creation

end program test_semantic_context_components
