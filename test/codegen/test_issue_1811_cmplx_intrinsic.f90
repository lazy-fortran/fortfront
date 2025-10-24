program test_issue_1811_cmplx_intrinsic
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
    implicit none
    logical :: passed

    passed = run_cmplx_intrinsic_case()
    if (passed) then
        write (*, '(A)') &
            'PASS: cmplx and aimag intrinsics avoid external declarations'
        stop 0
    end if

    write (error_unit, '(A)') &
        'FAIL: cmplx or aimag incorrectly marked as external'
    stop 1

contains

    function run_cmplx_intrinsic_case() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        call build_cmplx_intrinsic_source(source)
        call transform_lazy_fortran_string(source, generated, errors)

        if (.not. allocated(generated)) generated = ''
        if (.not. allocated(errors)) errors = ''

        passed = .true.
        if (len_trim(errors) > 0) then
            write (error_unit, '(A)') 'transform reported errors'
            write (error_unit, '(A)') trim(errors)
            passed = .false.
        end if

        if (index(generated, 'external :: cmplx') > 0) then
            write (error_unit, '(A)') &
                'unexpected external declaration for cmplx'
            passed = .false.
        end if

        if (index(generated, 'external :: aimag') > 0) then
            write (error_unit, '(A)') &
                'unexpected external declaration for aimag'
            passed = .false.
        end if

        if (index(generated, 'cmplx(3.0, 4.0)') == 0) then
            write (error_unit, '(A)') 'missing cmplx call in output'
            passed = .false.
        end if

        if (index(generated, 'aimag(z3)') == 0) then
            write (error_unit, '(A)') 'missing aimag call in output'
            passed = .false.
        end if
    end function run_cmplx_intrinsic_case

    subroutine build_cmplx_intrinsic_source(source)
        character(len=:), allocatable, intent(out) :: source

        source = 'program test_complex_numbers' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    complex :: z1, z2, z3' // new_line('a') // &
                 '    z1 = (1.0, 2.0)' // new_line('a') // &
                 '    z2 = cmplx(3.0, 4.0)' // new_line('a') // &
                 '    z3 = z1 + z2' // new_line('a') // &
                 '    print *, z1' // new_line('a') // &
                 '    print *, z2' // new_line('a') // &
                 '    print *, z3' // new_line('a') // &
                 '    print *, real(z3)' // new_line('a') // &
                 '    print *, aimag(z3)' // new_line('a') // &
                 'end program test_complex_numbers'
    end subroutine build_cmplx_intrinsic_source

end program test_issue_1811_cmplx_intrinsic
