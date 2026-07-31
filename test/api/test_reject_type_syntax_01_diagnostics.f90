program test_reject_type_syntax_01_diagnostics
    ! Issue #2891: malformed type and SELECT TYPE syntax must be rejected with a
    ! rule-specific diagnostic, while the corrected neighbouring form still
    ! compiles. The gfortran.dg fixtures named by the issue are mirrored under
    ! examples/f90/ with their exact basenames; the *_ok.f90 neighbours are the
    ! corrected forms and are verified against gfortran -std=f2018.
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use semantic_operating_mode, only: OPERATING_MODE_STRICT
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #2891: malformed type / SELECT TYPE rejection ==='

    call expect_rejected('class_is_1', 'CLASS IS specification', all_passed)
    call expect_rejected('type_is_1', 'TYPE IS specification', all_passed)
    call expect_rejected('pr91660_1', 'Malformed type-spec', all_passed)
    call expect_rejected('pr91715', 'CHARACTER declaration', all_passed)
    call expect_rejected('pr96099_1', 'IMPLICIT statement', all_passed)
    call expect_rejected('pr96099_2', 'IMPLICIT statement', all_passed)
    call expect_rejected('pr19936_3', 'COMPLEX constant', all_passed)

    call expect_accepted('class_is_1_ok', all_passed)
    call expect_accepted('type_is_1_ok', all_passed)
    call expect_accepted('pr91660_1_ok', all_passed)
    call expect_accepted('pr91715_ok', all_passed)
    call expect_accepted('pr96099_1_ok', all_passed)
    call expect_accepted('pr19936_3_ok', all_passed)

    ! Guard against over-rejection of the valid forms that share these code
    ! paths: kind selectors in IMPLICIT, complex literals and implied-dos in
    ! array constructors, and CHARACTER length selectors.
    call expect_accepted('implicit_letter_spec_forms', all_passed)
    ! A kind or length selector before the letter-spec list must not be read as
    ! a letter-spec list. fortfront still loses that letter list on emit
    ! (pre-existing, see examples/expected_failures.txt), so only the rules
    ! added here are asserted.
    call expect_no_rule_rejection('implicit_kind_selector_letter_specs', all_passed)
    call expect_accepted('issue_2390_module_implicit_rules', all_passed)
    call expect_accepted('complex_assignment_literals', all_passed)
    call expect_accepted('issue_1575_implied_do_array_constructor', all_passed)
    call expect_accepted('issue_1970_implied_do_array_constructor', all_passed)
    call expect_accepted('issue_2013_nested_implied_do_duplicate_var', all_passed)
    call expect_accepted('issue_1614_character_parameter_length', all_passed)
    call expect_accepted('issue_2559_select_type_case_variations', all_passed)

    print *
    if (all_passed) then
        print *, 'All issue #2891 rejection tests PASSED!'
        stop 0
    else
        print *, 'Some issue #2891 rejection tests FAILED!'
        error stop 1
    end if

contains

    subroutine transform_example(basename, output_code, error_msg)
        character(len=*), intent(in) :: basename
        character(len=:), allocatable, intent(out) :: output_code
        character(len=:), allocatable, intent(out) :: error_msg
        character(len=:), allocatable :: input_code
        type(transform_context_t) :: context

        call read_example('examples/f90/'//basename//'.f90', input_code)

        context%input_mode = INPUT_MODE_STANDARD
        context%operating_mode = OPERATING_MODE_STRICT
        context%has_filename = .true.
        context%source_name = basename

        call transform_with_context(input_code, output_code, error_msg, context)
    end subroutine transform_example

    subroutine expect_rejected(basename, expected_fragment, passed)
        character(len=*), intent(in) :: basename
        character(len=*), intent(in) :: expected_fragment
        logical, intent(inout) :: passed
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg

        call transform_example(basename, output_code, error_msg)

        if (len_trim(error_msg) == 0) then
            print *, 'FAIL: ', basename, ' was accepted but must be rejected'
            print *, '  output: ', trim(output_code)
            passed = .false.
            return
        end if

        if (index(error_msg, expected_fragment) == 0) then
            print *, 'FAIL: ', basename, ' rejected by the wrong rule'
            print *, '  expected fragment: ', expected_fragment
            print *, '  error: ', trim(error_msg)
            passed = .false.
            return
        end if

        print *, 'PASS: ', basename, ' rejected (', expected_fragment, ')'
    end subroutine expect_rejected

    ! Weaker guard for fixtures that trip unrelated pre-existing diagnostics:
    ! assert only that none of the rules added for issue #2891 fired.
    subroutine expect_no_rule_rejection(basename, passed)
        character(len=*), intent(in) :: basename
        logical, intent(inout) :: passed
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        character(len=24), parameter :: fragments(5) = [ &
            character(len=24) :: 'TYPE IS specification', 'CLASS IS specification', &
            'Malformed type-spec', 'CHARACTER declaration', 'IMPLICIT statement']
        integer :: i

        call transform_example(basename, output_code, error_msg)

        do i = 1, size(fragments)
            if (index(error_msg, trim(fragments(i))) > 0) then
                print *, 'FAIL: ', basename, ' hit rule ', trim(fragments(i))
                print *, '  error: ', trim(error_msg)
                passed = .false.
                return
            end if
        end do

        print *, 'PASS: ', basename, ' not rejected by any issue 2891 rule'
    end subroutine expect_no_rule_rejection

    subroutine expect_accepted(basename, passed)
        character(len=*), intent(in) :: basename
        logical, intent(inout) :: passed
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg

        call transform_example(basename, output_code, error_msg)

        if (len_trim(error_msg) /= 0) then
            print *, 'FAIL: ', basename, ' is valid but was rejected'
            print *, '  error: ', trim(error_msg)
            passed = .false.
            return
        end if

        print *, 'PASS: ', basename, ' accepted'
    end subroutine expect_accepted

    include '../common/read_example.inc'
end program test_reject_type_syntax_01_diagnostics
