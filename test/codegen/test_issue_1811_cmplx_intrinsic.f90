program test_issue_1811_cmplx_intrinsic
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit, input_unit, iostat_end, iostat_eor
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

    include '../common/read_example.inc'


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

        if (index(generated, 'cmplx(3.0, 4.0)') == 0 .and. &
            index(generated, 'cmplx(3.0_8, 4.0_8)') == 0) then
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

        call read_example('examples/f90/issue_1811_cmplx_intrinsic.f90', source)
    end subroutine build_cmplx_intrinsic_source

end program test_issue_1811_cmplx_intrinsic
