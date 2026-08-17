program test_reject_2987_proc_ptr_pdt_diagnostics
    ! Issue #2987: malformed procedure-pointer components and empty
    ! derived-type parameter lists must be rejected by the parser.
    !   proc_ptr_comp_3.f90: missing '::' after the attribute list and a
    !     duplicate POINTER attribute
    !   pdt_30.f90: an empty type parameter list after the type name
    ! The *_ok.f90 neighbours are the corrected forms and must still compile.
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use semantic_operating_mode, only: OPERATING_MODE_STRICT
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Issue #2987: malformed procedure-pointer / PDT rejection ==='

    call expect_rejected('issue_2987_proc_ptr_comp_3', 'Expected ''::''', all_passed)
    call expect_rejected('issue_2987_proc_ptr_comp_3', 'Duplicate POINTER', all_passed)
    call expect_rejected('issue_2987_pdt_30', 'type parameter list', all_passed)

    call expect_accepted('issue_2987_proc_ptr_comp_3_ok', all_passed)
    call expect_accepted('issue_2987_pdt_30_ok', all_passed)

    print *
    if (all_passed) then
        print *, 'All issue #2987 rejection tests PASSED!'
        stop 0
    else
        print *, 'Some issue #2987 rejection tests FAILED!'
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

    ! Reject the fixture, and make sure at least one of the given fragments
    ! appears in the diagnostic.
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
            print *, 'FAIL: ', basename, ' missing diagnostic fragment: ', &
                expected_fragment
            print *, '  error: ', trim(error_msg)
            passed = .false.
            return
        end if

        print *, 'PASS: ', basename, ' rejected (', expected_fragment, ')'
    end subroutine expect_rejected

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
end program test_reject_2987_proc_ptr_pdt_diagnostics
