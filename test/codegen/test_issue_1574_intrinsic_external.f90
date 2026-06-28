program test_issue_1574_intrinsic_external
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none
    logical :: passed

    passed = run_intrinsic_external_case()
    if (passed) then
        write (*, '(A)') &
            'PASS: intrinsic calls avoid external declarations'
        stop 0
    end if

    write (error_unit, '(A)') &
        'FAIL: intrinsic calls injected external declarations'
    stop 1

contains

    include '../common/read_example.inc'

    function run_intrinsic_external_case() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        call read_example('examples/f90/issue_1574_intrinsic_external.f90', source)

        call transform_lazy_fortran_string(source, generated, errors)

        if (.not. allocated(generated)) generated = ''
        if (.not. allocated(errors)) errors = ''

        passed = .true.
        if (len_trim(errors) > 0) then
            write (error_unit, '(A)') 'transform reported errors'
            write (error_unit, '(A)') trim(errors)
            passed = .false.
        end if

        if (index(generated, 'external :: maxval') > 0) then
            write (error_unit, '(A)') &
                'unexpected external declaration for maxval'
            passed = .false.
        end if

        if (index(generated, 'maxval(arr)') == 0) then
            write (error_unit, '(A)') 'missing maxval call in output'
            passed = .false.
        end if
    end function run_intrinsic_external_case

end program test_issue_1574_intrinsic_external
