program test_reject_end_01_interface_spec
    ! Rejection coverage for matching construct terminators (issue #2893).
    !
    ! Rule under test: F2018 R1503/R1504. When an end-interface-stmt carries a
    ! generic-spec it must be the generic-spec of the interface-stmt that opened
    ! the block, and an interface block without a generic-spec must not be
    ! closed with one.
    ! Represented by gfortran.dg/interface_operator_1.f90 and
    ! gfortran.dg/interface_operator_2.f90.
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    print *, '=== Reject mismatched END INTERFACE (issue #2893) ==='

    call test_operator_end_without_symbol_rejected(all_tests_passed)
    call test_operator_end_with_other_symbol_rejected(all_tests_passed)
    call test_named_end_with_other_name_rejected(all_tests_passed)
    call test_unnamed_interface_with_named_end_rejected(all_tests_passed)
    call test_operator_end_with_same_symbol_accepted(all_tests_passed)
    call test_operator_end_spelled_differently_accepted(all_tests_passed)
    call test_operator_bare_end_accepted(all_tests_passed)
    call test_named_end_case_insensitive_accepted(all_tests_passed)
    call test_assignment_end_accepted(all_tests_passed)
    call test_unnamed_interface_bare_end_accepted(all_tests_passed)

    if (all_tests_passed) then
        print *, 'All END INTERFACE rejection tests passed'
        stop 0
    else
        print *, 'Some END INTERFACE rejection tests failed'
        stop 1
    end if

contains

    function operator_block(spec, end_spec) result(source)
        character(len=*), intent(in) :: spec
        character(len=*), intent(in) :: end_spec
        character(len=:), allocatable :: source

        source = 'module m'//new_line('a')// &
            'interface '//spec//new_line('a')// &
            'logical function gt_t(a, b)'//new_line('a')// &
            'integer, intent(in) :: a, b'//new_line('a')// &
            'end function gt_t'//new_line('a')// &
            'end interface '//end_spec//new_line('a')// &
            'end module m'
    end function operator_block

    subroutine test_operator_end_without_symbol_rejected(passed)
        logical, intent(inout) :: passed

        print *, 'Testing END INTERFACE OPERATOR without symbol (rejected)...'
        call expect_frontend_error(operator_block('operator ( .gt. )', &
            'operator'), 'END INTERFACE operator(.gt.)', passed)
    end subroutine test_operator_end_without_symbol_rejected

    subroutine test_operator_end_with_other_symbol_rejected(passed)
        logical, intent(inout) :: passed

        print *, 'Testing END INTERFACE OPERATOR with other symbol (rejected)...'
        call expect_frontend_error(operator_block('operator ( .gt. )', &
            'operator (.lt.)'), 'END INTERFACE operator(.gt.)', passed)
    end subroutine test_operator_end_with_other_symbol_rejected

    subroutine test_named_end_with_other_name_rejected(passed)
        logical, intent(inout) :: passed

        print *, 'Testing END INTERFACE with a different name (rejected)...'
        call expect_frontend_error(operator_block('gen', 'other'), &
            'END INTERFACE gen', passed)
    end subroutine test_named_end_with_other_name_rejected

    subroutine test_unnamed_interface_with_named_end_rejected(passed)
        logical, intent(inout) :: passed

        print *, 'Testing named END INTERFACE for an unnamed block (rejected)...'
        call expect_frontend_error(operator_block('', 'gen'), &
            'must not name a generic spec', passed)
    end subroutine test_unnamed_interface_with_named_end_rejected

    subroutine test_operator_end_with_same_symbol_accepted(passed)
        logical, intent(inout) :: passed

        print *, 'Testing END INTERFACE OPERATOR with same symbol (accepted)...'
        call expect_frontend_accepts(operator_block('operator ( .gt. )', &
            'operator (.gt.)'), passed)
    end subroutine test_operator_end_with_same_symbol_accepted

    subroutine test_operator_end_spelled_differently_accepted(passed)
        logical, intent(inout) :: passed

        print *, 'Testing END INTERFACE OPERATOR (>) for (.gt.) (accepted)...'
        call expect_frontend_accepts(operator_block('operator ( .gt. )', &
            'operator (>)'), passed)
    end subroutine test_operator_end_spelled_differently_accepted

    subroutine test_operator_bare_end_accepted(passed)
        logical, intent(inout) :: passed

        print *, 'Testing bare END INTERFACE for an operator block (accepted)...'
        call expect_frontend_accepts(operator_block('operator ( .gt. )', ''), &
            passed)
    end subroutine test_operator_bare_end_accepted

    subroutine test_named_end_case_insensitive_accepted(passed)
        logical, intent(inout) :: passed

        print *, 'Testing END INTERFACE name matched case-insensitively...'
        call expect_frontend_accepts(operator_block('gen', 'GEN'), passed)
    end subroutine test_named_end_case_insensitive_accepted

    subroutine test_assignment_end_accepted(passed)
        logical, intent(inout) :: passed

        print *, 'Testing END INTERFACE ASSIGNMENT (=) (accepted)...'
        call expect_frontend_accepts(operator_block('assignment (=)', &
            'assignment (=)'), passed)
    end subroutine test_assignment_end_accepted

    subroutine test_unnamed_interface_bare_end_accepted(passed)
        logical, intent(inout) :: passed

        print *, 'Testing bare END INTERFACE for an unnamed block (accepted)...'
        call expect_frontend_accepts(operator_block('', ''), passed)
    end subroutine test_unnamed_interface_bare_end_accepted

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

end program test_reject_end_01_interface_spec
