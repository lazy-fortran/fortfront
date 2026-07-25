program test_reject_tbp_01_diagnostics
    ! Issue #2881: fortfront must reject a FINAL subroutine whose dummy argument
    ! is not a nonpolymorphic data object of the finalized type, and a
    ! type-bound procedure whose signature disagrees with the binding it
    ! overrides. Each negative fixture must produce the diagnostic of its own
    ! rule, and the corrected neighbour of each rule must stay accepted.
    use frontend_compiler_api, only: compiler_frontend_options_t, &
        compiler_frontend_result_t, compile_frontend_from_string
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call expect_rejected('examples/f90/finalize_22.f90', &
        'Argument of FINAL procedure')
    call expect_rejected('examples/f90/pr104572.f90', &
        'alternate return indicator')
    call expect_rejected('examples/f90/typebound_override_1.f90', &
        'Character length mismatch in function result')
    call expect_rejected('examples/f90/typebound_override_1.f90', &
        'Rank mismatch in function result')
    call expect_rejected('examples/f90/typebound_override_1.f90', &
        'declared with a constant character length')
    call expect_rejected('examples/f90/typebound_override_2.f90', &
        'INTENT mismatch in argument')
    call expect_rejected('examples/f90/typebound_override_4.f90', &
        'Type mismatch in argument')
    call expect_rejected('examples/f90/typebound_override_5.f90', &
        'Type mismatch in argument')

    call expect_accepted('examples/f90/finalize_22_valid.f90')
    call expect_accepted('examples/f90/pr104572_valid.f90')
    call expect_accepted('examples/f90/typebound_override_1_valid.f90')
    call expect_accepted('examples/f90/typebound_override_2_valid.f90')
    call expect_accepted('examples/f90/typebound_override_4_valid.f90')

    write (*, '(A,I0,A,I0,A)') 'Passed ', pass_count, ' out of ', &
        test_count, ' tests.'
    if (pass_count /= test_count) then
        write (error_unit, '(A)') 'FAIL'
        stop 1
    end if

contains

    include '../common/read_example.inc'

    ! The fixture must be rejected, and the diagnostic text must name the rule
    ! that rejected it rather than any diagnostic at all.
    subroutine expect_rejected(fixture, expected_fragment)
        character(len=*), intent(in) :: fixture
        character(len=*), intent(in) :: expected_fragment
        type(compiler_frontend_result_t) :: outcome
        character(len=:), allocatable :: diagnostics

        test_count = test_count + 1
        call analyze(fixture, outcome)
        diagnostics = ''
        if (allocated(outcome%diagnostic_text)) diagnostics = outcome%diagnostic_text

        if (.not. outcome%parse_ok) then
            write (*, '(A)') 'FAIL: '//fixture//' did not parse'
            return
        end if
        if (outcome%semantic_ok) then
            write (*, '(A)') 'FAIL: '//fixture//' was accepted'
            return
        end if
        if (index(diagnostics, expected_fragment) == 0) then
            write (*, '(A)') 'FAIL: '//fixture// &
                ' rejected without the expected diagnostic'
            write (*, '(A)') '  expected fragment: '//expected_fragment
            write (*, '(A)') '  diagnostics: '//diagnostics
            return
        end if

        pass_count = pass_count + 1
        write (*, '(A)') 'PASS: '//fixture//' rejected with '//expected_fragment
    end subroutine expect_rejected

    ! The corrected neighbour must still compile: a check that is too eager is
    ! worse than a missing one.
    subroutine expect_accepted(fixture)
        character(len=*), intent(in) :: fixture
        type(compiler_frontend_result_t) :: outcome
        character(len=:), allocatable :: diagnostics

        test_count = test_count + 1
        call analyze(fixture, outcome)
        diagnostics = ''
        if (allocated(outcome%diagnostic_text)) diagnostics = outcome%diagnostic_text

        if (.not. outcome%parse_ok) then
            write (*, '(A)') 'FAIL: '//fixture//' did not parse'
            return
        end if
        if (.not. outcome%semantic_ok) then
            write (*, '(A)') 'FAIL: '//fixture//' was rejected'
            write (*, '(A)') '  diagnostics: '//diagnostics
            return
        end if

        pass_count = pass_count + 1
        write (*, '(A)') 'PASS: '//fixture//' accepted'
    end subroutine expect_accepted

    subroutine analyze(fixture, outcome)
        character(len=*), intent(in) :: fixture
        type(compiler_frontend_result_t), intent(out) :: outcome
        type(compiler_frontend_options_t) :: options
        character(len=:), allocatable :: source

        call read_example(fixture, source)
        options%run_semantics = .true.
        call compile_frontend_from_string(source, outcome, options)
    end subroutine analyze

end program test_reject_tbp_01_diagnostics
