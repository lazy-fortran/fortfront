program test_issue_2753_unsigned_mixing_diagnostics
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use semantic_operating_mode, only: OPERATING_MODE_STRICT
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    print *, "=== Issue #2753: signed/unsigned mixing diagnostics ==="

    call test_assignment_mixing_requires_conversion()
    print *, ""
    call test_argument_mixing_requires_conversion()
    print *, ""
    print *, "Issue 2753 mixing diagnostics tests completed."

contains

    subroutine test_assignment_mixing_requires_conversion()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        type(transform_context_t) :: context

        call read_example( &
            'examples/f90/issue_2753_unsigned_assignment_mixing_error.f90', &
            input_code)

        context%input_mode = INPUT_MODE_STANDARD
        context%operating_mode = OPERATING_MODE_STRICT
        context%has_filename = .true.
        context%source_name = "issue_2753_unsigned_assignment_mixing_error"

        call transform_with_context(input_code, output_code, error_msg, context)

        if (len_trim(error_msg) == 0) then
            print *, "FAIL: expected signed/unsigned assignment mixing error"
            print *, "Transformed output:"
            print *, trim(output_code)
            error stop 1
        end if

        if (index(error_msg, "signed and unsigned") == 0) then
            print *, "FAIL: unexpected error message"
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        print *, "PASS: assignment mixing requires conversion"
    end subroutine test_assignment_mixing_requires_conversion

    subroutine test_argument_mixing_requires_conversion()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        type(transform_context_t) :: context

        call read_example( &
            'examples/f90/issue_2753_unsigned_argument_mixing_error.f90', &
            input_code)

        context%input_mode = INPUT_MODE_STANDARD
        context%operating_mode = OPERATING_MODE_STRICT
        context%has_filename = .true.
        context%source_name = "issue_2753_unsigned_argument_mixing_error"

        call transform_with_context(input_code, output_code, error_msg, context)

        if (len_trim(error_msg) == 0) then
            print *, "FAIL: expected signed/unsigned argument mixing error"
            print *, "Transformed output:"
            print *, trim(output_code)
            error stop 1
        end if

        if (index(error_msg, "signed and unsigned") == 0) then
            print *, "FAIL: unexpected error message"
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        print *, "PASS: argument mixing requires conversion"
    end subroutine test_argument_mixing_requires_conversion

    include 'common/read_example.inc'
end program test_issue_2753_unsigned_mixing_diagnostics
