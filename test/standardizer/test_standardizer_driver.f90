program test_standardizer_driver
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    print '(a)', '=== Standardizer driver coverage ==='

    call assert_declaration_insertion()
    call assert_program_wrapping_for_bare_statements()
    call assert_function_standardization()
    call assert_subroutine_intent_inference()

    print '(a)', 'All standardizer driver tests PASSED'

contains

    include '../common/read_example.inc'

    subroutine assert_declaration_insertion()
        character(len=:), allocatable :: source, output, errors

        call read_example('examples/lf/issue_1330_missing_declarations.lf', source)
        call transform_lazy_fortran_string(source, output, errors)
        call assert_no_errors('issue_1330_missing_declarations', errors)

        ! Driver must insert implicit none and declarations for inferred names.
        call require_contains(output, 'implicit none', &
            'declaration insertion: implicit none')
        call require_contains(output, ':: count', &
            'declaration insertion: integer count declared')
        call require_contains(output, ':: pi_estimate', &
            'declaration insertion: real pi_estimate declared')
        call require_contains(output, 'program main', &
            'program wrapping: program main header')
    end subroutine assert_declaration_insertion

    subroutine assert_program_wrapping_for_bare_statements()
        character(len=:), allocatable :: source, output, errors

        call read_example('examples/lf/assignment_simple.lf', source)
        call transform_lazy_fortran_string(source, output, errors)
        call assert_no_errors('assignment_simple', errors)

        call require_contains(output, 'program main', &
            'bare statements wrapped in program main')
        call require_contains(output, 'end program main', &
            'bare statements have matching end program')
        call require_contains(output, 'implicit none', &
            'wrapped program gets implicit none')
    end subroutine assert_program_wrapping_for_bare_statements

    subroutine assert_function_standardization()
        character(len=:), allocatable :: source, output, errors

        call read_example('examples/lf/basic_function.lf', source)
        call transform_lazy_fortran_string(source, output, errors)
        call assert_no_errors('basic_function', errors)

        call require_contains(output, 'integer function square', &
            'function standardization: explicit return type')
        call require_contains(output, 'intent(in) :: x', &
            'function standardization: parameter intent inferred')
    end subroutine assert_function_standardization

    subroutine assert_subroutine_intent_inference()
        character(len=:), allocatable :: source, output, errors

        call read_example('examples/lf/intent_mixed_subroutine.lf', source)
        call transform_lazy_fortran_string(source, output, errors)
        call assert_no_errors('intent_mixed_subroutine', errors)

        call require_contains(output, 'intent(inout)', &
            'subroutine intent inference: inout for read+write arg')
        call require_contains(output, 'intent(in)', &
            'subroutine intent inference: in for read-only arg')
    end subroutine assert_subroutine_intent_inference

    subroutine assert_no_errors(label, errors)
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: errors

        if (len_trim(errors) /= 0) then
            write (error_unit, '(a)') 'FAIL: transformation errors for ' // &
                trim(label) // ': ' // trim(errors)
            error stop 1
        end if
    end subroutine assert_no_errors

    subroutine require_contains(text, needle, label)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: needle
        character(len=*), intent(in) :: label

        if (index(text, needle) == 0) then
            write (error_unit, '(a)') 'FAIL: ' // trim(label) // &
                ' (substring not found: ' // trim(needle) // ')'
            write (error_unit, '(a)') '--- output ---'
            write (error_unit, '(a)') text
            write (error_unit, '(a)') '--- end ---'
            error stop 1
        end if
        print '(a)', 'PASS: ' // trim(label)
    end subroutine require_contains

end program test_standardizer_driver
