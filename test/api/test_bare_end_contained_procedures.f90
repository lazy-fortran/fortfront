program test_bare_end_contained_procedures
    ! A contained procedure may be terminated by a bare END statement
    ! (Fortran 2023 R1537/R1503). The CONTAINS scan used to keep searching
    ! for "end subroutine" past such an END, so every sibling procedure that
    ! followed was swallowed into the first one and never reached the host's
    ! body. IMPLICIT NONE (EXTERNAL) then reported the sibling as undeclared
    ! (gfortran.dg/associated_assumed_rank.f90).
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== bare END terminating contained procedures ==='

    call test_siblings_after_bare_end_are_collected(all_tests_passed)
    call test_mixed_terminators_collect_all_siblings(all_tests_passed)
    call test_undeclared_external_still_rejected(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All bare-END contained-procedure tests passed'
        stop 0
    else
        print *, 'Some bare-END contained-procedure tests failed'
        stop 1
    end if

contains

    subroutine test_siblings_after_bare_end_are_collected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'two contained subroutines, both ended by bare END...'
        source = 'implicit none (type, external)'//new_line('a')// &
            'call foo(1)'//new_line('a')// &
            'call bar(2)'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine foo(n)'//new_line('a')// &
            'integer, intent(in) :: n'//new_line('a')// &
            'print *, n'//new_line('a')// &
            'end'//new_line('a')// &
            'subroutine bar(n)'//new_line('a')// &
            'integer, intent(in) :: n'//new_line('a')// &
            'print *, n'//new_line('a')// &
            'end'//new_line('a')// &
            'end'
        call expect_frontend_success(source, passed)
    end subroutine test_siblings_after_bare_end_are_collected

    subroutine test_mixed_terminators_collect_all_siblings(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'bare END followed by an explicit END SUBROUTINE...'
        source = 'implicit none (type, external)'//new_line('a')// &
            'call first()'//new_line('a')// &
            'call second()'//new_line('a')// &
            'call third()'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine first()'//new_line('a')// &
            'print *, 1'//new_line('a')// &
            'end'//new_line('a')// &
            'subroutine second()'//new_line('a')// &
            'print *, 2'//new_line('a')// &
            'end subroutine second'//new_line('a')// &
            'subroutine third()'//new_line('a')// &
            'print *, 3'//new_line('a')// &
            'end'//new_line('a')// &
            'end'
        call expect_frontend_success(source, passed)
    end subroutine test_mixed_terminators_collect_all_siblings

    ! Negative control: collecting the siblings must not make the check
    ! accept a procedure that really is undeclared.
    subroutine test_undeclared_external_still_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'call to a procedure that is not contained (rejected)...'
        source = 'implicit none (type, external)'//new_line('a')// &
            'call foo(1)'//new_line('a')// &
            'call absent(2)'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine foo(n)'//new_line('a')// &
            'integer, intent(in) :: n'//new_line('a')// &
            'print *, n'//new_line('a')// &
            'end'//new_line('a')// &
            'end'
        call expect_frontend_error(source, 'absent', passed)
    end subroutine test_undeclared_external_still_rejected

    subroutine expect_frontend_success(source, passed)
        use frontend_compiler_api, only: compiler_frontend_options_t, &
            compiler_frontend_result_t, compile_frontend_from_string
        use semantic_input_mode, only: INPUT_MODE_STANDARD
        character(len=*), intent(in) :: source
        logical, intent(inout) :: passed
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)

        if (.not. result%success()) then
            print *, '  FAIL: valid source was rejected'
            print *, trim(result%diagnostic_text)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_success

    subroutine expect_frontend_error(source, fragment, passed)
        use frontend_compiler_api, only: compiler_frontend_options_t, &
            compiler_frontend_result_t, compile_frontend_from_string
        use semantic_input_mode, only: INPUT_MODE_STANDARD
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: fragment
        logical, intent(inout) :: passed
        type(compiler_frontend_options_t) :: options
        type(compiler_frontend_result_t) :: result

        options%run_semantics = .true.
        options%input_mode = INPUT_MODE_STANDARD
        options%standardize = .false.
        call compile_frontend_from_string(source, result, options)

        if (result%success()) then
            print *, '  FAIL: invalid source was accepted'
            passed = .false.
        else if (index(result%diagnostic_text, fragment) == 0) then
            print *, '  FAIL: diagnostic does not mention '//fragment
            print *, trim(result%diagnostic_text)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_error

end program test_bare_end_contained_procedures
