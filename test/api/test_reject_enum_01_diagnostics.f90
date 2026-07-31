program test_reject_enum_01_diagnostics
    ! Issue #2899: ENUM grammar and value constraints (F2003 R460 and 4.6).
    ! Each invalid form must produce a rule-specific source diagnostic, and the
    ! corrected neighbour of every rule must still be accepted.
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use semantic_operating_mode, only: OPERATING_MODE_STRICT
    use transformation_api, only: transform_with_context, transform_context_t
    implicit none

    integer :: failures

    failures = 0

    print *, "=== Issue #2899: ENUM rejection diagnostics ==="

    call expect_rejected('enum_2', 'only ENUMERATOR statements')
    call expect_rejected('enum_2', "'::' is required")
    call expect_rejected('enum_3', 'integer expression')
    call expect_rejected('enum_6', 'only ENUMERATOR statements')
    call expect_rejected('enum_7', 'may not be nested')
    call expect_rejected('enum_8', 'too big for its kind')

    call expect_accepted('enum_2_valid')
    call expect_accepted('enum_3_valid')
    call expect_accepted('enum_6_valid')
    call expect_accepted('enum_7_valid')
    call expect_accepted('enum_8_valid')

    if (failures > 0) then
        print *, 'FAIL: ', failures, ' ENUM diagnostic checks failed'
        error stop 1
    end if
    print *, 'PASS: ENUM constraint diagnostics'

contains

    subroutine compile_example(name, error_msg, output_code)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: error_msg
        character(len=:), allocatable, intent(out) :: output_code
        character(len=:), allocatable :: input_code
        type(transform_context_t) :: context

        call read_example('examples/f90/'//name//'.f90', input_code)

        context%input_mode = INPUT_MODE_STANDARD
        context%operating_mode = OPERATING_MODE_STRICT
        context%has_filename = .true.
        context%source_name = name

        call transform_with_context(input_code, output_code, error_msg, context)
    end subroutine compile_example

    subroutine expect_rejected(name, expected_fragment)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: expected_fragment
        character(len=:), allocatable :: error_msg, output_code

        call compile_example(name, error_msg, output_code)

        if (len_trim(error_msg) == 0) then
            print *, 'FAIL: ', name, ' was accepted but must be rejected'
            failures = failures + 1
            return
        end if
        if (index(error_msg, expected_fragment) == 0) then
            print *, 'FAIL: ', name, ' diagnostic lacks "', expected_fragment, '"'
            print *, 'Error: ', trim(error_msg)
            failures = failures + 1
            return
        end if
        print *, 'PASS: ', name, ' rejected with "', expected_fragment, '"'
    end subroutine expect_rejected

    subroutine expect_accepted(name)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: error_msg, output_code

        call compile_example(name, error_msg, output_code)

        if (len_trim(error_msg) /= 0) then
            print *, 'FAIL: ', name, ' is valid but was rejected'
            print *, 'Error: ', trim(error_msg)
            failures = failures + 1
            return
        end if
        if (index(output_code, 'enumerator') == 0) then
            print *, 'FAIL: ', name, ' lost its ENUM definition'
            print *, 'Output: ', trim(output_code)
            failures = failures + 1
            return
        end if
        print *, 'PASS: ', name, ' accepted'
    end subroutine expect_accepted

    include '../common/read_example.inc'
end program test_reject_enum_01_diagnostics
