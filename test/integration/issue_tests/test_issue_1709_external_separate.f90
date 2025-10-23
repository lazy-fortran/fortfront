program test_issue_1709_external_separate
    use frontend, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
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

    function run_external_function_case() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        source = 'program test_external' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    real, external :: my_func' // new_line('a') // &
                 '    real :: result' // new_line('a') // &
                 '' // new_line('a') // &
                 '    result = my_func(5.0)' // new_line('a') // &
                 '    print *, result' // new_line('a') // &
                 'end program test_external' // new_line('a') // &
                 '' // new_line('a') // &
                 'function my_func(x) result(y)' // new_line('a') // &
                 '    real, intent(in) :: x' // new_line('a') // &
                 '    real :: y' // new_line('a') // &
                 '    y = x * 2.0' // new_line('a') // &
                 'end function my_func'

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
