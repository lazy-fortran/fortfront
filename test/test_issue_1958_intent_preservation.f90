program test_issue_1958_intent_preservation
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_with_context, transform_context_t
    use frontend_transformation, only: INPUT_MODE_STANDARD
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    type(transform_context_t) :: context
    logical :: has_intent_in
    logical :: has_intent_out
    logical :: has_optional_flag
    logical :: test_passed

    print *, "=== Issue #1958: preserve intents for standard inputs ==="

    call read_example('examples/f90/issue_1958_intent_preservation.f90', &
                      input_code)

    context%input_mode = INPUT_MODE_STANDARD
    context%has_filename = .true.
    context%source_name = "test_issue_1958_input"

    call transform_with_context(input_code, output_code, error_msg, context)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transform_with_context returned error:", trim(error_msg)
        error stop 1
    end if

    has_intent_in = index(output_code, "integer, intent(in) :: a, b") > 0
    has_intent_out = index(output_code, "integer, intent(out) :: value") > 0
    has_optional_flag = index(output_code, "logical, intent(in), optional :: flag") > 0

    test_passed = has_intent_in .and. has_intent_out .and. has_optional_flag

    if (.not. has_intent_in) then
        print *, "FAIL: intent(in) attribute missing for parameters a, b"
    end if
    if (.not. has_intent_out) then
        print *, "FAIL: intent(out) attribute missing for parameter value"
    end if
    if (.not. has_optional_flag) then
        print *, "FAIL: optional intent attributes missing for flag"
    end if

    if (.not. test_passed) then
        print *, "Output code:"
        print *, trim(output_code)
        error stop 1
    end if

    print *, "PASS: intent attributes preserved for standard Fortran inputs"


contains


    include 'common/read_example.inc'
end program test_issue_1958_intent_preservation
