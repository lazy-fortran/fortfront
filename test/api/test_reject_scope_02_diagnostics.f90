program test_reject_scope_02_diagnostics
    ! Issue #2888 (reject-scope-02): a name used both as a derived-type name and
    ! as an entity name in the same scoping unit must be rejected with a source
    ! diagnostic, while the corrected neighbour stays accepted.
    !
    ! Oracle: gfortran rejects gfortran.dg/type_decl_4.f90 with
    ! "Symbol 'xx' at (1) also declared as a type at (2)" and accepts a program
    ! that merely declares a variable OF the type. The expectations below come
    ! from the standard rule (F2023 19.3.1), not from fortfront's own output.
    use fortfront, only: compiler_frontend_result_t, &
        compiler_frontend_options_t, compile_frontend_from_string, &
        INPUT_MODE_STANDARD
    implicit none

    logical :: all_passed

    print *, '=== Issue #2888: scope name-collision diagnostics ==='

    all_passed = .true.
    if (.not. test_type_and_variable_collision_rejected()) all_passed = .false.
    if (.not. test_corrected_neighbour_accepted()) all_passed = .false.
    if (.not. test_rejected('examples/f90/common_29.f90', &
        'incompatible object', "'t'")) all_passed = .false.
    if (.not. test_rejected('examples/f90/host_assoc_types_1.f90', &
        'incompatible object', "'vertex'")) all_passed = .false.
    if (.not. test_rejected('examples/f90/pr104351.f90', &
        'already defined', "'f'")) all_passed = .false.
    if (.not. test_rejected('examples/f90/pr77414.f90', &
        'already defined', "'pr77414_outer'")) all_passed = .false.
    if (.not. test_rejected('examples/f90/pr96102.f90', &
        'internal procedure of the same name', "'n'")) all_passed = .false.
    if (.not. test_rejected('examples/f90/used_types_25.f90', &
        'use association', "'t'")) all_passed = .false.
    if (.not. test_rejected('examples/f90/pr123375.f90', &
        'already accessible in the local scope', "'aa'")) all_passed = .false.
    if (.not. test_accepted('examples/f90/common_29_corrected.f90')) &
        all_passed = .false.
    if (.not. test_accepted('examples/f90/host_assoc_types_1_corrected.f90')) &
        all_passed = .false.
    if (.not. test_accepted('examples/f90/pr104351_corrected.f90')) &
        all_passed = .false.
    if (.not. test_accepted('examples/f90/pr77414_corrected.f90')) &
        all_passed = .false.
    if (.not. test_accepted('examples/f90/pr96102_corrected.f90')) &
        all_passed = .false.
    if (.not. test_accepted('examples/f90/used_types_25_corrected.f90')) &
        all_passed = .false.
    if (.not. test_accepted('examples/f90/pr123375_corrected.f90')) &
        all_passed = .false.

    if (all_passed) then
        print *, 'All reject-scope-02 diagnostics tests passed.'
        stop 0
    else
        print *, 'reject-scope-02 diagnostics tests FAILED.'
        stop 1
    end if

contains

    logical function test_type_and_variable_collision_rejected() result(ok)
        type(compiler_frontend_result_t) :: result
        character(len=:), allocatable :: source
        character(len=:), allocatable :: text

        ok = .true.
        call read_example('examples/f90/type_decl_4.f90', source)
        call compile_standard(source, result)

        if (result%success()) then
            print *, '  FAIL: type/variable name collision was accepted'
            ok = .false.
            return
        end if

        text = lowered(diagnostic_of(result))
        if (index(text, 'also declared as a type') == 0) then
            print *, '  FAIL: diagnostic is not the name-collision rule -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        if (index(text, "'xx'") == 0) then
            print *, '  FAIL: diagnostic does not name the symbol -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        print *, '  PASS: type/variable name collision rejected'
    end function test_type_and_variable_collision_rejected

    logical function test_corrected_neighbour_accepted() result(ok)
        type(compiler_frontend_result_t) :: result
        character(len=:), allocatable :: source

        ok = .true.
        call read_example('examples/f90/type_decl_4_corrected.f90', source)
        call compile_standard(source, result)

        if (.not. result%success()) then
            print *, '  FAIL: corrected neighbour rejected -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        print *, '  PASS: corrected neighbour still accepted'
    end function test_corrected_neighbour_accepted

    ! An invalid fixture must be rejected with a diagnostic of this rule family
    ! that names the offending symbol.
    logical function test_rejected(path, rule_text, symbol_text) result(ok)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: rule_text
        character(len=*), intent(in) :: symbol_text
        type(compiler_frontend_result_t) :: result
        character(len=:), allocatable :: source
        character(len=:), allocatable :: text

        ok = .true.
        call read_example(path, source)
        call compile_standard(source, result)
        if (result%success()) then
            print *, '  FAIL: accepted invalid fixture ', path
            ok = .false.
            return
        end if
        text = lowered(diagnostic_of(result))
        if (index(text, rule_text) == 0) then
            print *, '  FAIL: wrong rule for ', path, ' -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        if (index(text, symbol_text) == 0) then
            print *, '  FAIL: symbol not named for ', path, ' -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        print *, '  PASS: rejected ', path
    end function test_rejected

    ! A corrected neighbour must keep compiling.
    logical function test_accepted(path) result(ok)
        character(len=*), intent(in) :: path
        type(compiler_frontend_result_t) :: result
        character(len=:), allocatable :: source

        ok = .true.
        call read_example(path, source)
        call compile_standard(source, result)
        if (.not. result%success()) then
            print *, '  FAIL: rejected valid fixture ', path, ' -> ', &
                trim(diagnostic_of(result))
            ok = .false.
            return
        end if
        print *, '  PASS: accepted ', path
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

end program test_reject_scope_02_diagnostics
