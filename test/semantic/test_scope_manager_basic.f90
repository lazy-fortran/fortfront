program test_scope_manager_basic
    use scope_manager
    use type_system_unified
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    write (*, '(A)') "=== Scope Manager Basic Tests ==="

    call test_scope_creation()
    call test_scope_stack_creation()
    call test_scope_lookup_empty()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if
    stop 0

contains

    subroutine test_scope_creation()
        type(scope_t) :: scope

        test_count = test_count + 1

        write (*, '(A)') "Testing scope creation..."
        scope = create_scope(SCOPE_GLOBAL, "test")

        if (scope%scope_type == SCOPE_GLOBAL .and. scope%name == "test") then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Scope creation"
        else
            write (*, '(A)') "FAIL: Scope creation"
        end if
    end subroutine test_scope_creation

    subroutine test_scope_stack_creation()
        type(scope_stack_t) :: stack

        test_count = test_count + 1

        write (*, '(A)') "Testing scope stack creation..."
        stack = create_scope_stack()

        if (stack%depth == 1 .and. stack%capacity == 10) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Scope stack creation"
        else
            write (*, '(A)') "FAIL: Scope stack creation"
        end if
    end subroutine test_scope_stack_creation

    subroutine test_scope_lookup_empty()
        type(scope_t) :: scope
        type(poly_type_t), allocatable :: result

        test_count = test_count + 1

        write (*, '(A)') "Testing scope lookup on empty scope..."
        scope = create_scope(SCOPE_GLOBAL, "test")

        write (*, '(A)') "About to call scope%lookup..."
        write (*, '(A,I0)') "Scope env count: ", scope%env%count
        write (*, '(A,I0)') "Scope env capacity: ", scope%env%capacity
        write (*, '(A,L1)') "Names allocated: ", allocated(scope%env%names)
        write (*, '(A,L1)') "Schemes allocated: ", allocated(scope%env%schemes)

        call scope%lookup("nonexistent", result)

        write (*, '(A)') "Successfully called scope%lookup"

        if (.not. allocated(result)) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Empty scope lookup"
        else
            write (*, '(A)') "FAIL: Empty scope lookup should return unallocated"
        end if
    end subroutine test_scope_lookup_empty

end program test_scope_manager_basic
