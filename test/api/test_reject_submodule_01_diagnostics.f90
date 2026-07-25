program test_reject_submodule_01_diagnostics
    ! Rejection coverage for submodule declaration consistency.
    !
    ! Every negative fixture must be rejected with a diagnostic from this rule
    ! family, and every corrected neighbour must still be accepted. Over-eager
    ! rejection is the failure mode this test guards against, so the positive
    ! cases are as load bearing as the negative ones.
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    use frontend_compiler_api, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    implicit none

    logical :: all_passed

    all_passed = .true.

    ! Nested SUBMODULE declarations (F2018 R1116: a submodule is a program unit)
    call assert_rejected('examples/f90/submodule_twice.f90', &
        'SUBMODULE declaration is not allowed', all_passed)
    call assert_rejected('examples/f90/submodule_unexp.f90', &
        'SUBMODULE declaration is not allowed', all_passed)

    ! Separate module procedure with a dummy argument list (F2018 R1505)
    call assert_rejected('examples/f90/pr93423.f90', &
        'must not have a dummy argument list', all_passed)

    ! Separate module subprogram without the MODULE prefix (F2018 15.6.2.5)
    call assert_rejected('examples/f90/submodule_36.f90', &
        'requires the MODULE prefix', all_passed)

    ! Binding label that disagrees with the interface body (F2018 C1550)
    call assert_rejected('examples/f90/pr89943_3.f90', &
        'mismatch in BIND(C) names', all_passed)
    call assert_rejected('examples/f90/pr89943_4.f90', &
        'mismatch in BIND(C) names', all_passed)

    ! Corrected neighbours must keep compiling
    call assert_accepted('examples/f90/submodule_placement_valid.f90', all_passed)
    call assert_accepted('examples/f90/submodule_module_procedure_valid.f90', &
        all_passed)
    call assert_accepted('examples/f90/submodule_module_prefix_valid.f90', &
        all_passed)
    call assert_accepted('examples/f90/submodule_bind_c_name_valid.f90', all_passed)
    call assert_accepted('examples/f90/issue_1827_submodule_simple.f90', all_passed)
    call assert_accepted('examples/f90/issue_1827_submodule_with_contents.f90', &
        all_passed)

    if (.not. all_passed) error stop 1
    write (output_unit, '(A)') 'PASS: submodule rejection diagnostics'

contains

    include '../common/read_example.inc'

    subroutine compile_example(path, rejected, diagnostic)
        character(len=*), intent(in) :: path
        logical, intent(out) :: rejected
        character(len=:), allocatable, intent(out) :: diagnostic
        character(len=:), allocatable :: source
        type(compiler_frontend_result_t) :: result
        type(compiler_frontend_options_t) :: options

        call read_example(path, source)

        options%run_semantics = .true.
        call compile_frontend_from_string(source, result, options)

        rejected = .not. (result%parse_ok .and. result%semantic_ok)
        if (allocated(result%error_msg)) then
            diagnostic = result%error_msg
        else
            diagnostic = ''
        end if
    end subroutine compile_example

    subroutine assert_rejected(path, expected_fragment, passed)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: expected_fragment
        logical, intent(inout) :: passed
        character(len=:), allocatable :: diagnostic
        logical :: rejected

        call compile_example(path, rejected, diagnostic)

        if (.not. rejected) then
            write (error_unit, '(A)') 'FAIL: '//path//' was accepted'
            passed = .false.
            return
        end if

        if (index(diagnostic, expected_fragment) == 0) then
            write (error_unit, '(A)') 'FAIL: '//path// &
                ' missing expected diagnostic "'//expected_fragment//'"'
            write (error_unit, '(A)') trim(diagnostic)
            passed = .false.
        end if
    end subroutine assert_rejected

    subroutine assert_accepted(path, passed)
        character(len=*), intent(in) :: path
        logical, intent(inout) :: passed
        character(len=:), allocatable :: diagnostic
        logical :: rejected

        call compile_example(path, rejected, diagnostic)

        if (rejected) then
            write (error_unit, '(A)') 'FAIL: '//path//' was rejected'
            write (error_unit, '(A)') trim(diagnostic)
            passed = .false.
        end if
    end subroutine assert_accepted

end program test_reject_submodule_01_diagnostics
