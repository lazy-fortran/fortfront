program test_semantic_context_creation
    use semantic_analyzer
    use type_system_unified
    use scope_manager
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_manual_semantic_context_creation()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_manual_semantic_context_creation()
        type(semantic_context_t) :: ctx

        test_count = test_count + 1

        write (*, '(A)') "Creating semantic context manually step by step..."

        ! Initialize substitution (step 1)
        ctx%subst%count = 0
        write (*, '(A)') "Step 1: Substitution initialized"

        ! Initialize hierarchical scope stack (step 2)
        ctx%scopes = create_scope_stack()
        write (*, '(A)') "Step 2: Scope stack initialized"

        ! Initialize environment and next var id (step 3)
        ctx%next_var_id = 1
        write (*, '(A)') "Step 3: Next var ID initialized"

        write (*, '(A)') "Manual context creation completed"

        ! Check context is initialized
        if (ctx%next_var_id == 1) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Manual semantic context creation"
        else
            write (*, '(A)') "FAIL: Manual semantic context creation"
        end if

        write (*, '(A)') "About to exit subroutine (destructor will be called)"
    end subroutine test_manual_semantic_context_creation

end program test_semantic_context_creation
