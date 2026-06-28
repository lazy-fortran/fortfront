program test_issue_2591_unsigned_integers
    use transformation_api, only: transform_with_context, transform_context_t
    use frontend_transformation, only: INPUT_MODE_STANDARD
    implicit none

    print *, "=== Issue #2591: unsigned integers support ==="

    call test_preserves_unsigned_attribute()
    print *, ""
    call test_signed_unsigned_mixing_requires_conversion()
    print *, ""
    print *, "Issue 2591 unsigned integers tests completed."

contains

    subroutine test_preserves_unsigned_attribute()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        type(transform_context_t) :: context

        call read_example('examples/f90/issue_2591_unsigned_declarations.f90', &
            input_code)

        context%input_mode = INPUT_MODE_STANDARD
        context%has_filename = .true.
        context%source_name = "issue_2591_unsigned_declarations"

        call transform_with_context(input_code, output_code, error_msg, context)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, ", unsigned") == 0) then
            print *, "FAIL: unsigned attribute not preserved"
            print *, "Transformed output:"
            print *, trim(output_code)
            error stop 1
        end if

        print *, "PASS: unsigned attribute preserved"
    end subroutine test_preserves_unsigned_attribute

    subroutine test_signed_unsigned_mixing_requires_conversion()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        type(transform_context_t) :: context

        call read_example('examples/f90/issue_2591_unsigned_mixing_error.f90', &
            input_code)

        context%input_mode = INPUT_MODE_STANDARD
        context%has_filename = .true.
        context%source_name = "issue_2591_unsigned_mixing_error"

        call transform_with_context(input_code, output_code, error_msg, context)

        if (len_trim(error_msg) == 0) then
            print *, "FAIL: expected signed/unsigned mixing error"
            print *, "Transformed output:"
            print *, trim(output_code)
            error stop 1
        end if

        if (index(error_msg, "signed and unsigned") == 0) then
            print *, "FAIL: unexpected error message"
            print *, "Error:", trim(error_msg)
            error stop 1
        end if

        print *, "PASS: signed/unsigned mixing requires conversion"
    end subroutine test_signed_unsigned_mixing_requires_conversion

    include 'common/read_example.inc'
end program test_issue_2591_unsigned_integers
