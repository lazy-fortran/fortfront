program test_issue_2829_poly_forall_vars
    ! Issue #2829 (e): create_poly_type now converts the input type_var_t array
    ! into stored handles, recoverable via get_poly_forall_vars, instead of
    ! discarding them as an empty set.
    use type_system_unified, only: mono_type_t, poly_type_t, type_var_t, &
        create_mono_type, create_type_var, &
        create_poly_type, get_poly_forall_vars, TREAL
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_empty_forall()
    call test_two_forall_vars()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", &
        test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_empty_forall()
        type(mono_type_t) :: mono
        type(poly_type_t) :: pt
        type(type_var_t), allocatable :: vars(:)

        test_count = test_count + 1
        mono = create_mono_type(TREAL)
        pt = create_poly_type(forall_vars=[type_var_t ::], mono=mono)
        vars = get_poly_forall_vars(pt)
        if (allocated(vars) .and. size(vars) == 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: empty forall set round-trips as empty"
        else
            write (*, '(A)') "FAIL: empty forall set not preserved"
        end if
    end subroutine test_empty_forall

    subroutine test_two_forall_vars()
        type(mono_type_t) :: mono
        type(poly_type_t) :: pt
        type(type_var_t) :: input_vars(2)
        type(type_var_t), allocatable :: vars(:)
        logical :: ok

        test_count = test_count + 1
        input_vars(1) = create_type_var(11, "a")
        input_vars(2) = create_type_var(22, "b")
        mono = create_mono_type(TREAL)
        pt = create_poly_type(forall_vars=input_vars, mono=mono)

        vars = get_poly_forall_vars(pt)
        ok = size(vars) == 2
        if (ok) ok = vars(1)%id == 11
        if (ok) ok = trim(vars(1)%name) == "a"
        if (ok) ok = vars(2)%id == 22
        if (ok) ok = trim(vars(2)%name) == "b"

        if (ok) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: forall vars converted and recovered"
        else
            write (*, '(A)') "FAIL: forall vars not converted correctly"
        end if
    end subroutine test_two_forall_vars

end program test_issue_2829_poly_forall_vars
