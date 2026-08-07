program test_issue_2993_implicit_none_diagnostics
    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend_compiler_api, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    use semantic_input_mode, only: INPUT_MODE_LAZY, INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: source
    type(compiler_frontend_options_t) :: options
    type(compiler_frontend_result_t) :: result

    call read_example('examples/f90/issue_2993_implicit_none_undeclared.f90', &
        source)
    options = compiler_frontend_options_t()
    options%input_mode = INPUT_MODE_LAZY
    options%run_semantics = .true.
    call compile_frontend_from_string(source, result, options)
    if (.not. result%parse_ok) call fail('invalid example did not parse')
    if (.not. result%semantic_ok) then
        call fail('Lazy mode rejected an inferred name: '// &
            trim(result%diagnostic_text))
    end if

    call read_example('examples/f90/issue_2993_implicit_none_valid.f90', source)
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        call fail('valid declarations and associations were rejected: '// &
            trim(result%diagnostic_text))
    end if

    ! Standard mode is the strict boundary: an explicit selection must reject
    ! the same undeclared reference that Lazy mode intentionally infers.
    options%input_mode = INPUT_MODE_STANDARD
    call read_example('examples/f90/issue_2993_implicit_none_undeclared.f90', &
        source)
    call compile_frontend_from_string(source, result, options)
    if (result%semantic_ok) call fail('standard mode accepted undeclared zzz')
    call assert_contains(result%diagnostic_text, &
        "Name 'zzz' is not declared under IMPLICIT NONE")

    call read_example('examples/f90/issue_2993_implicit_none_valid.f90', source)
    call compile_frontend_from_string(source, result, options)
    if (.not. result%success()) then
        call fail('standard mode rejected valid bindings: '// &
            trim(result%diagnostic_text))
    end if

    print *, 'PASS: IMPLICIT NONE rejects undeclared names without over-rejecting bindings'

contains

    include '../common/read_example.inc'

    subroutine assert_contains(text, needle)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: needle

        if (index(text, needle) == 0) then
            call fail('diagnostic did not contain: '//trim(needle))
        end if
    end subroutine assert_contains

    subroutine fail(message)
        character(len=*), intent(in) :: message

        write (error_unit, '(A)') 'FAIL: '//trim(message)
        error stop 1
    end subroutine fail

end program test_issue_2993_implicit_none_diagnostics
