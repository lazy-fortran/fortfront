program test_issue_1895_use_interface_external
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
    implicit none
    logical :: passed

    passed = run_use_interface_external_case()
    if (passed) then
        write (*, '(A)') &
            'PASS: interface exports avoid external declarations'
        stop 0
    end if

    write (error_unit, '(A)') &
        'FAIL: interface exports injected external declarations'
    stop 1

contains

    function run_use_interface_external_case() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        source = 'module my_module' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    interface my_func' // new_line('a') // &
                 '        module procedure func_impl' // new_line('a') // &
                 '    end interface my_func' // new_line('a') // &
                 'contains' // new_line('a') // &
                 '    function func_impl(x) result(y)' // new_line('a') // &
                 '        integer, intent(in) :: x' // new_line('a') // &
                 '        integer :: y' // new_line('a') // &
                 '        y = x * 2' // new_line('a') // &
                 '    end function func_impl' // new_line('a') // &
                 'end module my_module' // new_line('a') // &
                 '' // new_line('a') // &
                 'program main' // new_line('a') // &
                 '    use my_module' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer :: result' // new_line('a') // &
                 '    result = my_func(5)' // new_line('a') // &
                 '    print *, result' // new_line('a') // &
                 'end program main'

        call transform_lazy_fortran_string(source, generated, errors)

        if (.not. allocated(generated)) generated = ''
        if (.not. allocated(errors)) errors = ''

        passed = .true.
        if (len_trim(errors) > 0) then
            write (error_unit, '(A)') 'transform reported errors'
            write (error_unit, '(A)') trim(errors)
            passed = .false.
        end if

        if (index(generated, 'external :: my_func') > 0) then
            write (error_unit, '(A)') &
                'unexpected external declaration for my_func'
            passed = .false.
        end if

        if (index(generated, 'result = my_func(5)') == 0) then
            write (error_unit, '(A)') 'missing my_func call in output'
            passed = .false.
        end if
    end function run_use_interface_external_case

end program test_issue_1895_use_interface_external
