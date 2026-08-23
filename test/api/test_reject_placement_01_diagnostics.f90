program test_reject_placement_01_diagnostics
    ! Rejection coverage for constructs in forbidden program sections
    ! (issue #2896).
    !
    ! Statement placement: a statement that the standard confines to one kind
    ! of program section must be rejected where it cannot appear, and the
    ! corrected neighbouring form must stay accepted. The fixtures are reduced
    ! from the gfortran.dg files that record each constraint.
    !
    ! Rule under test: F2018 C858. The PROTECTED attribute may appear only in
    ! the specification part of a module. A main program has no module to
    ! protect anything from, so the attribute is invalid there.
    ! Represented by the constraint of gfortran.dg/pr68054.f90.
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== Reject misplaced declarations (issue #2896) ==='

    call test_misplaced_fixtures_rejected(all_tests_passed)
    call test_corrected_neighbours_accepted(all_tests_passed)
    call test_protected_in_main_program_rejected(all_tests_passed)
    call test_protected_in_implicit_main_rejected(all_tests_passed)
    call test_protected_in_multi_declaration_rejected(all_tests_passed)
    call test_protected_in_module_accepted(all_tests_passed)
    call test_protected_after_module_contains_rejected(all_tests_passed)
    call test_protected_after_program_contains_rejected(all_tests_passed)
    call test_protected_statement_in_module_accepted(all_tests_passed)
    call test_contained_subroutine_accepted(all_tests_passed)
    call test_plain_declaration_in_main_program_accepted(all_tests_passed)
    call test_save_in_main_program_accepted(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All declaration placement rejection tests passed'
        stop 0
    else
        print *, 'Some declaration placement rejection tests failed'
        stop 1
    end if

contains

    ! Every listed invalid form produces a diagnostic naming its own rule.
    subroutine test_misplaced_fixtures_rejected(passed)
        logical, intent(inout) :: passed

        call expect_example_error('examples/f90/blockdata_8.f90', &
            'STATEMENT FUNCTION statement is not allowed inside of BLOCK DATA', &
            passed)
        call expect_example_error( &
            'examples/f90/misplaced_implicit_character.f90', &
            'IMPLICIT statement at (1) cannot follow data declaration', passed)
        call expect_example_error('examples/f90/misplaced_statement.f90', &
            'Unexpected SEQUENCE statement', passed)
        call expect_example_error('examples/f90/pdt_33.f90', &
            'Unexpected derived type declaration', passed)
        call expect_example_error('examples/f90/pr61669.f90', &
            'data declaration statement at (1) cannot appear after '// &
            'executable statements', passed)
        call expect_example_error('examples/f90/pr68054.f90', &
            'PROTECTED attribute', passed)
        call expect_example_error('examples/f90/pr68319.f90', &
            'cannot appear within an INTERFACE body', passed)
        call expect_example_error('examples/f90/stfunc_5.f90', &
            'Unexpected STATEMENT FUNCTION statement', passed)
        call expect_example_error('examples/f90/unexpected_interface.f90', &
            'Unexpected INTERFACE statement', passed)
    end subroutine test_misplaced_fixtures_rejected

    ! Negative control: the corrected neighbour of every rule keeps compiling.
    subroutine test_corrected_neighbours_accepted(passed)
        logical, intent(inout) :: passed

        call expect_example_accepted('examples/f90/issue_1578_block_data.f90', &
            passed)
        call expect_example_accepted( &
            'examples/f90/implicit_none_single.f90', passed)
        call expect_example_accepted( &
            'examples/f90/placement_sections_corrected.f90', passed)
        call expect_example_accepted( &
            'examples/f90/issue_1353_derived_type.f90', passed)
        call expect_example_accepted( &
            'examples/f90/issue_1821_protected_attribute.f90', passed)
        call expect_example_accepted( &
            'examples/f90/issue_2489_interface_in_procedure.f90', passed)
        call expect_example_accepted( &
            'examples/f90/issue_2280_statement_function.f90', passed)
        call expect_example_accepted('examples/f90/interface_only_block.f90', &
            passed)
    end subroutine test_corrected_neighbours_accepted

    subroutine expect_example_error(path, expected, passed)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: expected
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        call read_example(path, source)
        print *, 'Testing rejection of ', path
        call expect_frontend_error(source, expected, passed)
    end subroutine expect_example_error

    subroutine expect_example_accepted(path, passed)
        character(len=*), intent(in) :: path
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        call read_example(path, source)
        print *, 'Testing acceptance of ', path
        call expect_frontend_accepts(source, passed)
    end subroutine expect_example_accepted

    subroutine test_protected_in_main_program_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing PROTECTED in an explicit main program (rejected)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'real, protected :: x'//new_line('a')// &
            'x = 1'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, 'PROTECTED attribute', passed)
    end subroutine test_protected_in_main_program_rejected

    subroutine test_protected_in_implicit_main_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing PROTECTED in an implicit main program (rejected)...'
        source = 'real, protected :: x'//new_line('a')// &
            'end'
        call expect_frontend_error(source, 'specification part of a module', &
            passed)
    end subroutine test_protected_in_implicit_main_rejected

    subroutine test_protected_in_multi_declaration_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing PROTECTED on a multi declaration (rejected)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'real, protected :: x, y'//new_line('a')// &
            'x = 1'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, 'PROTECTED attribute', passed)
    end subroutine test_protected_in_multi_declaration_rejected

    subroutine test_protected_in_module_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing PROTECTED in a module specification part (accepted)...'
        source = 'module m'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'real, protected :: x'//new_line('a')// &
            'end module m'
        call expect_frontend_accepts(source, passed)
    end subroutine test_protected_in_module_accepted

    subroutine test_protected_after_module_contains_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing PROTECTED after module CONTAINS (rejected)...'
        source = 'module a'//new_line('a')// &
            'contains'//new_line('a')// &
            'protected x'//new_line('a')// &
            'end module a'
        call expect_frontend_error(source, &
            'PROTECTED statement is only allowed in the specification part', &
            passed)
    end subroutine test_protected_after_module_contains_rejected

    subroutine test_protected_after_program_contains_rejected(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing PROTECTED after program CONTAINS (rejected)...'
        source = 'program p'//new_line('a')// &
            'contains'//new_line('a')// &
            'protected x'//new_line('a')// &
            'end program p'
        call expect_frontend_error(source, &
            'PROTECTED statement is only allowed in the specification part', &
            passed)
    end subroutine test_protected_after_program_contains_rejected

    subroutine test_protected_statement_in_module_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing PROTECTED in a module specification part (accepted)...'
        source = 'module m'//new_line('a')// &
            'real :: x'//new_line('a')// &
            'protected x'//new_line('a')// &
            'end module m'
        call expect_frontend_accepts(source, passed)
    end subroutine test_protected_statement_in_module_accepted

    subroutine test_contained_subroutine_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing an ordinary contained subroutine (accepted)...'
        source = 'program p'//new_line('a')// &
            'contains'//new_line('a')// &
            'subroutine s'//new_line('a')// &
            'end subroutine s'//new_line('a')// &
            'end program p'
        call expect_frontend_accepts(source, passed)
    end subroutine test_contained_subroutine_accepted

    subroutine test_plain_declaration_in_main_program_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing a plain declaration in a main program (accepted)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'real :: x'//new_line('a')// &
            'x = 1'//new_line('a')// &
            'end program p'
        call expect_frontend_accepts(source, passed)
    end subroutine test_plain_declaration_in_main_program_accepted

    subroutine test_save_in_main_program_accepted(passed)
        logical, intent(inout) :: passed
        character(len=:), allocatable :: source

        print *, 'Testing a SAVE declaration in a main program (accepted)...'
        source = 'program p'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'real, save :: x'//new_line('a')// &
            'x = 1'//new_line('a')// &
            'end program p'
        call expect_frontend_accepts(source, passed)
    end subroutine test_save_in_main_program_accepted

    subroutine expect_frontend_error(source, expected, passed)
        use frontend_compiler_api, only: compiler_frontend_options_t, &
            compiler_frontend_result_t, compile_frontend_from_string
        use semantic_input_mode, only: INPUT_MODE_STANDARD
        character(len=*), intent(in) :: source
        character(len=*), intent(in) :: expected
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
            return
        end if
        if (index(result%error_msg, expected) == 0) then
            print *, '  FAIL: diagnostic missing expected text: ', expected
            print *, trim(result%error_msg)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_error

    subroutine expect_frontend_accepts(source, passed)
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
            if (allocated(result%error_msg)) print *, trim(result%error_msg)
            passed = .false.
        else
            print *, '  PASS'
        end if
    end subroutine expect_frontend_accepts

    include '../common/read_example.inc'

end program test_reject_placement_01_diagnostics
