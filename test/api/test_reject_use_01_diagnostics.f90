program test_reject_use_01_diagnostics
    ! Issue #2887 (reject-use-01): a scoping unit that accesses the same module
    ! under two conflicting module natures must be rejected with a source
    ! diagnostic, while the corrected neighbour stays accepted.
    !
    ! Oracle: gfortran rejects gfortran.dg/iso_fortran_env_4.f90 with
    ! "conflicts with intrinsic module" / "conflicts with non-intrinsic module"
    ! and accepts the corrected neighbour. The expectations below are derived
    ! from the standard rule (F2023 14.2.2), not from fortfront's own output.
    use fortfront, only: compiler_frontend_result_t, &
        compiler_frontend_options_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    implicit none

    logical :: all_passed

    print *, '=== Issue #2887: USE module-nature conflict diagnostics ==='

    all_passed = .true.
    if (.not. test_conflicting_natures_rejected()) all_passed = .false.
    if (.not. test_corrected_neighbour_accepted()) all_passed = .false.
    if (.not. test_missing_export_rejected('use_9', 'operator(.func.)')) &
        all_passed = .false.
    if (.not. test_missing_export_rejected('use_19', 'operator(/)')) &
        all_passed = .false.
    if (.not. test_missing_export_rejected('operator_6', 'operator(.none.)')) &
        all_passed = .false.
    if (.not. test_missing_export_rejected('interface_operator_3', &
        'operator(/=)')) all_passed = .false.
    if (.not. test_accepted('use_9_corrected')) all_passed = .false.
    if (.not. test_accepted('use_19_corrected')) all_passed = .false.
    if (.not. test_accepted('operator_6_corrected')) all_passed = .false.
    if (.not. test_accepted('interface_operator_3_corrected')) &
        all_passed = .false.

    if (all_passed) then
        print *, 'All reject-use-01 diagnostics tests passed.'
        stop 0
    else
        print *, 'reject-use-01 diagnostics tests FAILED.'
        stop 1
    end if

contains

    logical function test_conflicting_natures_rejected() result(ok)
        type(compiler_frontend_result_t) :: result
        character(len=:), allocatable :: source
        character(len=:), allocatable :: text

        ok = .true.
        call read_example('examples/f90/iso_fortran_env_4.f90', source)
        call compile_standard(source, result)

        if (result%success()) then
            print *, '  FAIL: conflicting module natures were accepted'
            ok = .false.
            return
        end if

        text = lowered(diagnostic_of(result))
        if (index(text, 'module nature') == 0) then
            print *, '  FAIL: diagnostic is not the module-nature rule -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        if (index(text, 'iso_fortran_env') == 0) then
            print *, '  FAIL: diagnostic does not name the module -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        print *, '  PASS: conflicting module natures rejected'
    end function test_conflicting_natures_rejected

    logical function test_corrected_neighbour_accepted() result(ok)
        type(compiler_frontend_result_t) :: result
        character(len=:), allocatable :: source

        ok = .true.
        call read_example('examples/f90/iso_fortran_env_4_corrected.f90', source)
        call compile_standard(source, result)

        if (.not. result%success()) then
            print *, '  FAIL: corrected neighbour rejected -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        print *, '  PASS: corrected neighbour still accepted'
    end function test_corrected_neighbour_accepted

    ! A USE ONLY list may only access entities the module really exports
    ! (F2023 14.2.2). The expected entity text is the canonical generic spec.
    logical function test_missing_export_rejected(basename, entity) result(ok)
        character(len=*), intent(in) :: basename
        character(len=*), intent(in) :: entity
        type(compiler_frontend_result_t) :: result
        character(len=:), allocatable :: source
        character(len=:), allocatable :: text

        ok = .true.
        call read_example('examples/f90/'//basename//'.f90', source)
        call compile_standard(source, result)

        if (result%success()) then
            print *, '  FAIL: accepted missing export in ', basename
            ok = .false.
            return
        end if

        text = lowered(diagnostic_of(result))
        if (index(text, 'not found in module') == 0) then
            print *, '  FAIL: wrong rule for ', basename, ' -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        if (index(text, lowered(entity)) == 0) then
            print *, '  FAIL: diagnostic does not name ', entity, ' -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        print *, '  PASS: missing export rejected in ', basename
    end function test_missing_export_rejected

    logical function test_accepted(basename) result(ok)
        character(len=*), intent(in) :: basename
        type(compiler_frontend_result_t) :: result
        character(len=:), allocatable :: source

        ok = .true.
        call read_example('examples/f90/'//basename//'.f90', source)
        call compile_standard(source, result)
        if (.not. result%success()) then
            print *, '  FAIL: rejected valid ', basename, ' -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        print *, '  PASS: accepted ', basename
    end function test_accepted

    subroutine compile_standard(source, result)
        character(len=*), intent(in) :: source
        type(compiler_frontend_result_t), intent(out) :: result
        type(compiler_frontend_options_t) :: options

        options%input_mode = INPUT_MODE_STANDARD
        call compile_frontend_from_string(source, result, options)
    end subroutine compile_standard

    function diagnostic_of(result) result(text)
        type(compiler_frontend_result_t), intent(in) :: result
        character(len=:), allocatable :: text

        if (allocated(result%error_msg)) then
            text = result%error_msg
        else
            text = ''
        end if
    end function diagnostic_of

    function lowered(text) result(out)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: out
        integer :: i, code

        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                out(i:i) = achar(code + 32)
            else
                out(i:i) = text(i:i)
            end if
        end do
    end function lowered

    include '../common/read_example.inc'

end program test_reject_use_01_diagnostics
