program test_issue_1709_external_separate
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit, input_unit, iostat_end, iostat_eor
    implicit none
    logical :: passed

    passed = run_external_function_case()
    if (passed) then
        write (*, '(A)') 'PASS: External functions remain separate compilation units'
        stop 0
    end if

    write (error_unit, '(A)') 'FAIL: External functions incorrectly moved to CONTAINS'
    stop 1

contains

    include '../../common/read_example.inc'


    function run_external_function_case() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        call read_example('examples/f90/issue_1709_external_separate.f90', source)

        call transform_lazy_fortran_string(source, generated, errors)

        if (.not. allocated(generated)) generated = ''
        if (.not. allocated(errors)) errors = ''

        passed = .true.

        if (len_trim(errors) > 0) then
            write (error_unit, '(A)') 'Transform reported errors:'
            write (error_unit, '(A)') trim(errors)
            passed = .false.
        end if

        if (index(generated, 'contains') > 0) then
            if (index(generated, 'end program') > index(generated, 'contains')) then
                write (error_unit, '(A)') &
                    'FAIL: Function incorrectly moved into program CONTAINS block'
                passed = .false.
            end if
        end if

        if (index(generated, 'external :: my_func') == 0) then
            write (error_unit, '(A)') 'FAIL: EXTERNAL attribute removed'
            passed = .false.
        end if

        if (index(generated, 'function my_func') == 0) then
            write (error_unit, '(A)') 'FAIL: Function definition missing'
            passed = .false.
        end if

        if (index(generated, 'end program') > 0) then
            if (index(generated, 'function my_func') > index(generated, 'end program')) then
            else
                write (error_unit, '(A)') &
                    'FAIL: Function not after end program (not separate unit)'
                passed = .false.
            end if
        end if
    end function run_external_function_case

end program test_issue_1709_external_separate
