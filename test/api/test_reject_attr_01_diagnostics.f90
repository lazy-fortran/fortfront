program test_reject_attr_01_diagnostics
    ! Issue #2895: duplicate or incompatible declaration attributes must be
    ! rejected with a source diagnostic, while the corrected neighbouring
    ! forms stay accepted.
    use frontend_transformation, only: INPUT_MODE_STANDARD
    use semantic_operating_mode, only: OPERATING_MODE_STRICT
    use transformation_api, only: transform_with_context, transform_context_t
    use declaration_attribute_utils, only: declaration_attribute_info_t, &
        attribute_validation_t, reset_declaration_attributes, &
        set_declaration_intent, validate_attribute_addition
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    integer :: failures

    failures = 0

    call expect_rejected('examples/f90/parameter_save.f90', &
        'SAVE attribute conflicts with PARAMETER attribute')
    call expect_rejected('examples/f90/public_private_module.f90', &
        'PRIVATE attribute conflicts with PUBLIC attribute')
    call expect_rejected('examples/f90/pr77583.f90', &
        'Duplicate SAVE attribute specified')
    call expect_rejected('examples/f90/protected_9.f90', &
        'PARAMETER attribute conflicts with PROTECTED attribute')
    call expect_rejected('examples/f90/contiguous_12.f90', &
        'CONTIGUOUS attribute requires an array')
    call expect_rejected('examples/f90/external_initializer.f90', &
        'EXTERNAL attribute conflicts with initialization')

    call expect_accepted('examples/f90/attribute_conflicts_corrected.f90')
    call expect_accepted('examples/f90/external_procedure_declaration.f90')

    call check_value_intent_rule()
    call check_compatible_combinations()

    if (failures /= 0) then
        write (error_unit, '(A,I0,A)') 'FAIL: ', failures, ' check(s) failed'
        error stop 1
    end if
    write (*, '(A)') 'PASS: reject-attr-01 diagnostics'

contains

    subroutine expect_rejected(path, expected_fragment)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: expected_fragment
        character(len=:), allocatable :: error_msg

        call compile_example(path, error_msg)

        if (len_trim(error_msg) == 0) then
            failures = failures + 1
            write (error_unit, '(A)') 'FAIL: expected rejection for ' // path
            return
        end if

        if (index(error_msg, expected_fragment) == 0) then
            failures = failures + 1
            write (error_unit, '(A)') 'FAIL: wrong diagnostic for ' // path
            write (error_unit, '(A)') '  expected: ' // expected_fragment
            write (error_unit, '(A)') '  actual:   ' // trim(error_msg)
        end if
    end subroutine expect_rejected

    subroutine expect_accepted(path)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: error_msg

        call compile_example(path, error_msg)

        if (len_trim(error_msg) /= 0) then
            failures = failures + 1
            write (error_unit, '(A)') 'FAIL: valid source rejected: ' // path
            write (error_unit, '(A)') '  actual: ' // trim(error_msg)
        end if
    end subroutine expect_accepted

    subroutine compile_example(path, error_msg)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: error_msg
        character(len=:), allocatable :: input_code, output_code
        type(transform_context_t) :: context

        call read_example(path, input_code)

        context%input_mode = INPUT_MODE_STANDARD
        context%operating_mode = OPERATING_MODE_STRICT
        context%has_filename = .true.
        context%source_name = path

        call transform_with_context(input_code, output_code, error_msg, context)
        if (.not. allocated(error_msg)) error_msg = ''
    end subroutine compile_example

    ! VALUE is compatible with INTENT(IN) and with nothing else. The rule can
    ! only be exercised on a dummy argument, and parser diagnostics raised
    ! inside a procedure body are not surfaced by the driver yet, so this is
    ! checked directly against the validator.
    subroutine check_value_intent_rule()
        type(declaration_attribute_info_t) :: attr
        type(attribute_validation_t) :: validation

        call reset_declaration_attributes(attr)
        attr%is_value = .true.
        validation = validate_attribute_addition(attr, 'intent(out)')
        call require_invalid(validation, 'VALUE with INTENT(OUT)')

        call reset_declaration_attributes(attr)
        attr%is_value = .true.
        validation = validate_attribute_addition(attr, 'intent(inout)')
        call require_invalid(validation, 'VALUE with INTENT(INOUT)')

        call reset_declaration_attributes(attr)
        call set_declaration_intent(attr, 'out')
        validation = validate_attribute_addition(attr, 'value')
        call require_invalid(validation, 'INTENT(OUT) then VALUE')

        call reset_declaration_attributes(attr)
        attr%is_value = .true.
        validation = validate_attribute_addition(attr, 'intent(in)')
        call require_valid(validation, 'VALUE with INTENT(IN)')
    end subroutine check_value_intent_rule

    ! Guard against over-rejection: these pairs are legal Fortran and must not
    ! be flagged by the validator.
    subroutine check_compatible_combinations()
        type(declaration_attribute_info_t) :: attr
        type(attribute_validation_t) :: validation

        call reset_declaration_attributes(attr)
        attr%is_parameter = .true.
        attr%has_global_dimensions = .true.
        validation = validate_attribute_addition(attr, 'public')
        call require_valid(validation, 'PARAMETER with DIMENSION and PUBLIC')

        call reset_declaration_attributes(attr)
        attr%is_allocatable = .true.
        validation = validate_attribute_addition(attr, 'target')
        call require_valid(validation, 'ALLOCATABLE with TARGET')

        call reset_declaration_attributes(attr)
        attr%is_pointer = .true.
        validation = validate_attribute_addition(attr, 'contiguous')
        call require_valid(validation, 'POINTER with CONTIGUOUS')

        call reset_declaration_attributes(attr)
        attr%is_optional = .true.
        validation = validate_attribute_addition(attr, 'intent(inout)')
        call require_valid(validation, 'OPTIONAL with INTENT(INOUT)')

        call reset_declaration_attributes(attr)
        attr%is_save = .true.
        validation = validate_attribute_addition(attr, 'volatile')
        call require_valid(validation, 'SAVE with VOLATILE')
    end subroutine check_compatible_combinations

    subroutine require_invalid(validation, label)
        type(attribute_validation_t), intent(in) :: validation
        character(len=*), intent(in) :: label

        if (validation%valid) then
            failures = failures + 1
            write (error_unit, '(A)') 'FAIL: expected rejection of ' // label
            return
        end if
        if (.not. allocated(validation%message)) then
            failures = failures + 1
            write (error_unit, '(A)') 'FAIL: no message for ' // label
        end if
    end subroutine require_invalid

    subroutine require_valid(validation, label)
        type(attribute_validation_t), intent(in) :: validation
        character(len=*), intent(in) :: label

        if (validation%valid) return
        failures = failures + 1
        write (error_unit, '(A)') 'FAIL: valid combination rejected: ' // label
        if (allocated(validation%message)) then
            write (error_unit, '(A)') '  actual: ' // validation%message
        end if
    end subroutine require_valid

    include '../common/read_example.inc'

end program test_reject_attr_01_diagnostics
