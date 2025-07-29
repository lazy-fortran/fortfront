program test_function_type_creation
    use type_system_hm
    use iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_basic_function_type()
    call test_poly_type_creation()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_basic_function_type()
        type(mono_type_t) :: real_type, fun_type

        test_count = test_count + 1

        write (*, '(A)') "Creating basic real type..."
        real_type = create_mono_type(TREAL)
        write (*, '(A)') "Real type created successfully"

        write (*, '(A)') "Creating function type real -> real..."
        fun_type = create_fun_type(real_type, real_type)
        write (*, '(A)') "Function type created successfully"

        ! Check function type
        if (fun_type%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Basic function type creation"
        else
            write (*, '(A)') "FAIL: Function type creation"
        end if

        write (*, '(A)') "About to exit (function type destructor will be called)"
    end subroutine test_basic_function_type

    subroutine test_poly_type_creation()
        type(mono_type_t) :: real_type, fun_type
        type(poly_type_t) :: poly_type

        test_count = test_count + 1

        write (*, '(A)') "Creating real type and function type for poly type..."
        real_type = create_mono_type(TREAL)
        fun_type = create_fun_type(real_type, real_type)
        write (*, '(A)') "Types for poly type created"

        write (*, '(A)') "Creating polymorphic type scheme..."
        poly_type = create_poly_type(forall_vars=[type_var_t::], mono=fun_type)
        write (*, '(A)') "Polymorphic type created successfully"

        ! Check poly type
        if (poly_type%mono%kind == TFUN) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: Polymorphic type creation"
        else
            write (*, '(A)') "FAIL: Polymorphic type creation"
        end if

        write (*, '(A)') "About to exit (poly type destructor will be called)"
    end subroutine test_poly_type_creation

end program test_function_type_creation
