program test_standardizer_attributes_monomorphization
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    print '(a)', '=== Standardizer attributes and monomorphization coverage ==='

    call assert_allocatable_preserved()
    call assert_pointer_declared_before_use()
    call assert_monomorphized_variants()

    print '(a)', 'All standardizer attribute and monomorphization tests PASSED'

contains

    include '../common/read_example.inc'

    subroutine assert_allocatable_preserved()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: errors

        call read_example('examples/lf/issue_1534_allocatable.lf', source)
        call transform_lazy_fortran_string(source, output, errors)
        call assert_no_errors('issue_1534_allocatable', errors)

        call require_contains(output, 'allocatable :: vec(:)', &
            'allocatable attribute uses deferred shape')
        call require_contains(output, 'allocate(vec(3))', &
            'allocatable allocation statement preserved')
        call require_absent(output, ':: vec(3)', &
            'allocatable declaration does not use allocation extent')
    end subroutine assert_allocatable_preserved

    subroutine assert_pointer_declared_before_use()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: errors
        integer :: declaration_pos
        integer :: assignment_pos

        call read_example('examples/lf/issue_2072_pointer_not_declared.lf', source)
        call transform_lazy_fortran_string(source, output, errors)
        call assert_no_errors('issue_2072_pointer_not_declared', errors)

        declaration_pos = index(output, 'integer, pointer :: ptr')
        assignment_pos = index(output, 'ptr => null()')

        if (declaration_pos == 0) then
            call fail_with_output('pointer declaration generated', output)
        end if
        if (assignment_pos == 0) then
            call fail_with_output('pointer assignment preserved', output)
        end if
        if (declaration_pos > assignment_pos) then
            call fail_with_output('pointer declaration precedes assignment', output)
        end if

        print '(a)', 'PASS: pointer declaration precedes pointer assignment'
    end subroutine assert_pointer_declared_before_use

    subroutine assert_monomorphized_variants()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: errors

        call read_example('examples/lf/monomorphization_add_three_types.lf', source)
        call transform_lazy_fortran_string(source, output, errors)
        call assert_no_errors('monomorphization_add_three_types', errors)

        call require_contains(output, 'module auto_add', &
            'monomorphization module generated')
        call require_contains(output, 'interface add', &
            'monomorphization generic interface generated')
        call require_contains(output, 'add__i32_i32', &
            'integer specialization generated')
        call require_contains(output, 'add__r64_r64', &
            'real64 specialization generated')
        call require_contains(output, 'add__c64_c64', &
            'complex specialization generated')
        call require_absent(output, 'external :: add', &
            'monomorphized procedure avoids external declaration')
    end subroutine assert_monomorphized_variants

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
            call fail_with_output(trim(label) // ' missing: ' // trim(needle), text)
        end if

        print '(a)', 'PASS: ' // trim(label)
    end subroutine require_contains

    subroutine require_absent(text, needle, label)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: needle
        character(len=*), intent(in) :: label

        if (index(text, needle) > 0) then
            call fail_with_output(trim(label) // ' present: ' // trim(needle), text)
        end if

        print '(a)', 'PASS: ' // trim(label)
    end subroutine require_absent

    subroutine fail_with_output(message, output)
        character(len=*), intent(in) :: message
        character(len=*), intent(in) :: output

        write (error_unit, '(a)') 'FAIL: ' // trim(message)
        write (error_unit, '(a)') '--- output ---'
        write (error_unit, '(a)') output
        write (error_unit, '(a)') '--- end ---'
        error stop 1
    end subroutine fail_with_output

end program test_standardizer_attributes_monomorphization
