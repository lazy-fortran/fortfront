program test_minimal_type_system
    use type_system_hm
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_create_mono_types()
    call test_copy_mono_types()
    call test_type_variable_creation()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_create_mono_types()
        type(mono_type_t) :: int_type, real_type, char_type

        test_count = test_count + 1

        ! Create basic types
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        char_type = create_mono_type(TCHAR, char_size=10)

        ! Check types are created correctly
        if (int_type%kind == TINT .and. real_type%kind == TREAL .and. char_type%kind == TCHAR) then
            if (char_type%size == 10) then
                pass_count = pass_count + 1
                write (*, '(A)') "PASS: Create basic mono types"
            else
                write (*, '(A)') "FAIL: Character type size incorrect"
            end if
        else
            write (*, '(A)') "FAIL: Basic mono type creation"
        end if
    end subroutine test_create_mono_types

    subroutine test_copy_mono_types()
        type(mono_type_t) :: original, copied

        test_count = test_count + 1

        ! Create original type
        original = create_mono_type(TREAL)

        ! Test deep copy
        copied = original%deep_copy()

        ! Check copy is correct
        if (copied%kind == TREAL .and. copied%kind == original%kind) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Deep copy mono types"
        else
            write (*, '(A)') "FAIL: Deep copy mono types"
        end if
    end subroutine test_copy_mono_types

    subroutine test_type_variable_creation()
        type(type_var_t) :: var1, var2
        type(mono_type_t) :: var_type1, var_type2

        test_count = test_count + 1

        ! Create type variables
        var1%name = "a"
        var1%id = 1
        var2%name = "b"
        var2%id = 2

        ! Create mono types with type variables
        var_type1 = create_mono_type(TVAR, var=var1)
        var_type2 = create_mono_type(TVAR, var=var2)

        ! Check type variables are different
        if (var_type1%kind == TVAR .and. var_type2%kind == TVAR) then
            if (var_type1%var%id /= var_type2%var%id) then
                pass_count = pass_count + 1
                write (*, '(A)') "PASS: Type variable creation"
            else
                write (*, '(A)') "FAIL: Type variables have same ID"
            end if
        else
            write (*, '(A)') "FAIL: Type variable creation kinds"
        end if
    end subroutine test_type_variable_creation

end program test_minimal_type_system
