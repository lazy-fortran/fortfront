program test_reject_purity_01_diagnostics
    ! Issue #2885: PURE/ELEMENTAL/RECURSIVE restrictions are enforced.
    ! Every negative fixture must be rejected with a diagnostic that names the
    ! violated rule, and the corrected neighbour of every rule must still be
    ! accepted. The oracle is gfortran: each negative fixture mirrors a
    ! gfortran.dg test carrying a dg-error for the same constraint, and each
    ! positive fixture is a form gfortran compiles.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_with_context, transform_context_t, &
        INPUT_MODE_STANDARD
    implicit none

    logical :: all_passed

    all_passed = .true.

    call check_rejected('examples/f90/class_dummy_5.f90', &
        'polymorphic INTENT(OUT) dummy in PURE', 'polymorphic', all_passed)
    call check_rejected('examples/f90/do_concurrent_3.f90', &
        'impure intrinsic subroutine in DO CONCURRENT', 'DO CONCURRENT', &
        all_passed)
    call check_rejected('examples/f90/elemental_args_check_2.f90', &
        'dummy procedure in ELEMENTAL', 'procedure dummy argument', all_passed)
    call check_rejected('examples/f90/elemental_pointer_1.f90', &
        'POINTER result of ELEMENTAL function', 'pointer result', all_passed)
    call check_rejected('examples/f90/elemental_result_1.f90', &
        'array result of ELEMENTAL function', 'array result', all_passed)
    call check_rejected('examples/f90/impure_assignment_2.f90', &
        'definition of a PURE function dummy', 'variable definition context', &
        all_passed)
    call check_rejected('examples/f90/impure_assignment_1.f90', &
        'impure defined assignment in PURE procedure', &
        'defined assignment statement', all_passed)
    call check_rejected('examples/f90/pure_formal_proc_3.f90', &
        'impure dummy procedure of PURE procedure', 'must also be PURE', &
        all_passed)
    call check_rejected('examples/f90/recursive_check_3.f90', &
        'duplicate prefix specifier', 'duplicate', all_passed)

    call check_accepted('examples/f90/class_dummy_5_valid.f90', &
        'polymorphic dummy without INTENT(OUT)', all_passed)
    call check_accepted('examples/f90/do_concurrent_3_valid.f90', &
        'impure intrinsic outside DO CONCURRENT', all_passed)
    call check_accepted('examples/f90/elemental_args_check_2_valid.f90', &
        'dummy procedure of a non-ELEMENTAL PURE procedure', all_passed)
    call check_accepted('examples/f90/elemental_pointer_1_valid.f90', &
        'scalar non-pointer ELEMENTAL result', all_passed)
    call check_accepted('examples/f90/elemental_result_1_valid.f90', &
        'scalar ELEMENTAL result beside an array-valued function', all_passed)
    call check_accepted('examples/f90/impure_assignment_2_valid.f90', &
        'PURE function that only reads its dummy', all_passed)
    call check_accepted('examples/f90/pure_formal_proc_3_valid.f90', &
        'PURE dummy procedure of a PURE function', all_passed)
    call check_accepted('examples/f90/recursive_check_3_valid.f90', &
        'distinct prefix specifiers', all_passed)

    if (all_passed) then
        write (*, '(a)') 'PASS: purity rejection diagnostics enforced'
    else
        stop 1
    end if

contains

    subroutine check_rejected(path, label, expected_text, passed)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: expected_text
        logical, intent(inout) :: passed
        character(len=:), allocatable :: error_msg

        call run_frontend(path, error_msg)

        if (.not. error_reported(error_msg)) then
            write (error_unit, '(a)') 'FAIL: '//label//' not rejected'
            passed = .false.
            return
        end if
        if (index(error_msg, expected_text) == 0) then
            write (error_unit, '(a)') 'FAIL: '//label// &
                ' rejected without the expected diagnostic: '//error_msg
            passed = .false.
        end if
    end subroutine check_rejected

    subroutine check_accepted(path, label, passed)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: label
        logical, intent(inout) :: passed
        character(len=:), allocatable :: error_msg

        call run_frontend(path, error_msg)

        if (error_reported(error_msg)) then
            write (error_unit, '(a)') 'FAIL: '//label//' rejected: '//error_msg
            passed = .false.
        end if
    end subroutine check_accepted

    subroutine run_frontend(path, error_msg)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: error_msg
        type(transform_context_t) :: context
        character(len=:), allocatable :: source, transformed

        call read_example(path, source)
        context%input_mode = INPUT_MODE_STANDARD
        call transform_with_context(source, transformed, error_msg, context)
        if (.not. allocated(error_msg)) error_msg = ''
    end subroutine run_frontend

    function error_reported(error_msg) result(reported)
        character(len=*), intent(in) :: error_msg
        logical :: reported

        reported = len_trim(error_msg) > 0
    end function error_reported

    include '../common/read_example.inc'
end program test_reject_purity_01_diagnostics
