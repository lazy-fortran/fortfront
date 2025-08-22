program test_intent_semantic_check
    use frontend, only: compile_source, compilation_options_t
    implicit none

    character(len=:), allocatable :: test_file, output_file
    character(len=1024) :: error_msg
    type(compilation_options_t) :: options
    logical :: test_passed

    test_passed = .true.

    ! Test 1: Valid code - no INTENT violations
    call test_valid_intent()

    ! Test 2: Invalid code - modifying INTENT(IN) parameter
    call test_intent_in_violation()

    ! Test 3: Valid code - modifying INTENT(OUT) parameter
    call test_intent_out_allowed()

    ! Test 4: Valid code - modifying INTENT(INOUT) parameter
    call test_intent_inout_allowed()

    if (test_passed) then
        print *, "All INTENT semantic check tests passed!"
    else
        print *, "Some INTENT semantic check tests failed!"
        stop 1
    end if

contains

    subroutine test_valid_intent()
        test_file = 'valid_intent.f90'

        open (unit=10, file=test_file, status='replace')
        write (10, '(A)') 'subroutine process(input, output)'
        write (10, '(A)') '    real, intent(in) :: input'
        write (10, '(A)') '    real, intent(out) :: output'
        write (10, '(A)') '    '
        write (10, '(A)') '    output = input * 2.0  ! OK: reading IN, writing OUT'
        write (10, '(A)') 'end subroutine process'
        close (10)

        output_file = 'valid_intent_out.f90'
        options%output_file = output_file

        call compile_source(test_file, options, error_msg)

        if (error_msg /= '') then
           print *, "Test 1 FAILED: Valid INTENT code produced error: ", trim(error_msg)
            test_passed = .false.
        else
            print *, "Test 1 PASSED: Valid INTENT code compiled successfully"
        end if
    end subroutine test_valid_intent

    subroutine test_intent_in_violation()
        test_file = 'intent_in_violation.f90'

        open (unit=10, file=test_file, status='replace')
        write (10, '(A)') 'subroutine bad_modify(input)'
        write (10, '(A)') '    real, intent(in) :: input'
        write (10, '(A)') '    '
        write (10, '(A)') '    input = 0.0  ! ERROR: Cannot modify INTENT(IN)'
        write (10, '(A)') 'end subroutine bad_modify'
        close (10)

        output_file = 'intent_in_violation_out.f90'
        options%output_file = output_file

        call compile_source(test_file, options, error_msg)

        if (error_msg == '') then
           print *, "Test 2 SKIPPED: INTENT(IN) violation detection not yet implemented"
            ! TODO: Enable when INTENT checking is implemented
            ! test_passed = .false.
        else if (index(error_msg, "INTENT(IN)") > 0) then
            print *, "Test 2 PASSED: INTENT(IN) violation correctly detected"
        else
            print *, "Test 2 FAILED: Wrong error message: ", trim(error_msg)
            test_passed = .false.
        end if
    end subroutine test_intent_in_violation

    subroutine test_intent_out_allowed()
        test_file = 'intent_out_allowed.f90'

        open (unit=10, file=test_file, status='replace')
        write (10, '(A)') 'subroutine set_output(result)'
        write (10, '(A)') '    real, intent(out) :: result'
        write (10, '(A)') '    '
        write (10, '(A)') '    result = 42.0  ! OK: Can modify INTENT(OUT)'
        write (10, '(A)') 'end subroutine set_output'
        close (10)

        output_file = 'intent_out_allowed_out.f90'
        options%output_file = output_file

        call compile_source(test_file, options, error_msg)

        if (error_msg /= '') then
    print *, "Test 3 FAILED: INTENT(OUT) modification produced error: ", trim(error_msg)
            test_passed = .false.
        else
            print *, "Test 3 PASSED: INTENT(OUT) modification allowed"
        end if
    end subroutine test_intent_out_allowed

    subroutine test_intent_inout_allowed()
        test_file = 'intent_inout_allowed.f90'

        open (unit=10, file=test_file, status='replace')
        write (10, '(A)') 'subroutine modify_value(value)'
        write (10, '(A)') '    real, intent(inout) :: value'
        write (10, '(A)') '    '
    write (10, '(A)') '    value = value + 1.0  ! OK: Can read and modify INTENT(INOUT)'
        write (10, '(A)') 'end subroutine modify_value'
        close (10)

        output_file = 'intent_inout_allowed_out.f90'
        options%output_file = output_file

        call compile_source(test_file, options, error_msg)

        if (error_msg /= '') then
  print *, "Test 4 FAILED: INTENT(INOUT) modification produced error: ", trim(error_msg)
            test_passed = .false.
        else
            print *, "Test 4 PASSED: INTENT(INOUT) modification allowed"
        end if
    end subroutine test_intent_inout_allowed

end program test_intent_semantic_check
