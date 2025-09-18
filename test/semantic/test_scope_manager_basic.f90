program test_scope_manager_basic
    use scope_manager
    use type_system_unified
    use identifier_table, only: identifier_table_get
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    write (*, '(A)') "=== Scope Manager Basic Tests ==="

    call test_scope_creation()
    call test_scope_stack_creation()
    call test_scope_lookup_empty()
    call test_identifier_interning_reuse()

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
        call create_scope(scope, SCOPE_GLOBAL, "test")

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
        call create_scope_stack(stack)

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
        call create_scope(scope, SCOPE_GLOBAL, "test")

        write (*, '(A)') "About to call scope%lookup..."
        write (*, '(A,I0)') "Scope env count: ", scope%env%count
        write (*, '(A,I0)') "Scope env capacity: ", scope%env%capacity
        write (*, '(A)') "Fixed arrays (always allocated)"

        call scope%lookup("nonexistent", result)

        write (*, '(A)') "Successfully called scope%lookup"

        if (.not. allocated(result)) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Empty scope lookup"
        else
            write (*, '(A)') "FAIL: Empty scope lookup should return unallocated"
        end if
    end subroutine test_scope_lookup_empty

    subroutine test_identifier_interning_reuse()
        type(scope_stack_t) :: stack
        type(mono_type_t) :: mono
        type(poly_type_t) :: scheme
        integer :: initial_count
        integer :: foo_id_first, foo_id_second
        character(len=:), allocatable :: interned_name

        test_count = test_count + 1

        write (*, '(A)') 'Testing identifier interning reuse...'
        call create_scope_stack(stack)

        mono = create_mono_type(TINT)
        scheme = create_poly_type([type_var_t::], mono)

        associate(env => stack%scopes(stack%depth)%env)
        initial_count = env%count
        call stack%define('foo', scheme)
        if (env%count /= initial_count + 1) then
            write (*, '(A)') 'FAIL: First definition did not increment count'
            return
        end if

        foo_id_first = env%name_ids(1)
        interned_name = identifier_table_get(stack%identifier_storage, foo_id_first)
        if (interned_name /= 'foo') then
            write (*, '(A)') 'FAIL: Interned name mismatch for first definition'
            return
        end if

        call stack%define('foo', scheme)
        foo_id_second = env%name_ids(1)
        if (env%count /= initial_count + 1) then
            write (*, '(A)') 'FAIL: Duplicate definition created extra entry'
            return
        end if
        if (foo_id_second /= foo_id_first) then
            write (*, '(A)') 'FAIL: Duplicate definition produced new identifier id'
            return
        end if

        call stack%define('bar', scheme)
        if (env%count /= initial_count + 2) then
            write (*, '(A)') 'FAIL: Second unique definition did not increment count'
            return
        end if
        if (env%name_ids(2) == foo_id_first) then
            write (*, '(A)') 'FAIL: Distinct identifiers share the same intern id'
            return
        end if

        end associate

        pass_count = pass_count + 1
        write (*, '(A)') 'PASS: Identifier interning reuse'
    end subroutine test_identifier_interning_reuse

end program test_scope_manager_basic
