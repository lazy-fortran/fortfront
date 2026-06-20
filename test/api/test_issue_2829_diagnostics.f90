program test_issue_2829_diagnostics
    ! Issue #2829 (a): get_diagnostics extracts real diagnostics from the
    ! semantic context error collection instead of always returning empty.
    use fortfront_utils, only: get_diagnostics
    use fortfront_types, only: diagnostic_t, DIAGNOSTIC_ERROR, &
                               DIAGNOSTIC_WARNING, DIAGNOSTIC_INFO
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use error_handling, only: ERROR_ERROR, ERROR_WARNING, ERROR_INFO, result_t
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0

    call test_empty_context()
    call test_populated_context()

    write (*, '(A,I0,A,I0,A)') "Passed ", pass_count, " out of ", &
        test_count, " tests."
    if (pass_count /= test_count) then
        write (error_unit, '(A)') "FAIL"
        stop 1
    end if

contains

    subroutine test_empty_context()
        type(semantic_context_t) :: ctx
        type(diagnostic_t), allocatable :: diags(:)

        test_count = test_count + 1
        call create_semantic_context(ctx)
        diags = get_diagnostics(ctx)
        if (allocated(diags) .and. size(diags) == 0) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: empty context yields zero diagnostics"
        else
            write (*, '(A)') "FAIL: empty context should yield zero diagnostics"
        end if
    end subroutine test_empty_context

    subroutine test_populated_context()
        type(semantic_context_t) :: ctx
        type(diagnostic_t), allocatable :: diags(:)
        type(result_t) :: info
        logical :: ok

        test_count = test_count + 1
        call create_semantic_context(ctx)
        call ctx%errors%add_error("boom", severity=ERROR_ERROR, &
                                  component="semantic_analyzer")
        call ctx%errors%add_error("careful", severity=ERROR_WARNING)
        info%success = .true.
        info%severity = ERROR_INFO
        info%error_message = "note"
        call ctx%errors%add_result(info)

        diags = get_diagnostics(ctx)

        ok = size(diags) == 3
        if (ok) ok = diags(1)%severity == DIAGNOSTIC_ERROR
        if (ok) ok = diags(1)%message == "boom"
        if (ok) ok = allocated(diags(1)%category)
        if (ok) ok = diags(1)%category == "semantic_analyzer"
        if (ok) ok = diags(2)%severity == DIAGNOSTIC_WARNING
        if (ok) ok = diags(2)%message == "careful"
        if (ok) ok = diags(3)%severity == DIAGNOSTIC_INFO

        if (ok) then
            pass_count = pass_count + 1
            write (*, '(A)') "PASS: diagnostics extracted with mapped severities"
        else
            write (*, '(A)') "FAIL: diagnostics not extracted correctly"
        end if
    end subroutine test_populated_context

end program test_issue_2829_diagnostics
