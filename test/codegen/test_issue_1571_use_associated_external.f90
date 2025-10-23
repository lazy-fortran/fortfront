program test_issue_1571_use_associated_external
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
    implicit none
    logical :: passed

    passed = run_use_associated_external_case()
    if (passed) then
        write (*, '(A)') &
            'PASS: use-associated functions avoid external declarations'
        stop 0
    end if

    write (error_unit, '(A)') &
        'FAIL: use-associated functions injected external declarations'
    stop 1

contains

    function run_use_associated_external_case() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        source = 'module math_funcs' // new_line('a') // &
                 'contains' // new_line('a') // &
                 '    elemental function add_one(x) result(y)' // new_line('a') // &
                 '        integer, intent(in) :: x' // new_line('a') // &
                 '        integer :: y' // new_line('a') // &
                 '        y = x + 1' // new_line('a') // &
                 '    end function add_one' // new_line('a') // &
                 'end module math_funcs' // new_line('a') // &
                 '' // new_line('a') // &
                 'program test_pure' // new_line('a') // &
                 '    use math_funcs' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer :: arr(3)' // new_line('a') // &
                 '    arr = [1, 2, 3]' // new_line('a') // &
                 '    arr = add_one(arr)' // new_line('a') // &
                 '    print *, arr' // new_line('a') // &
                 'end program test_pure'

        call transform_lazy_fortran_string(source, generated, errors)

        if (.not. allocated(generated)) generated = ''
        if (.not. allocated(errors)) errors = ''

        passed = .true.
        if (len_trim(errors) > 0) then
            write (error_unit, '(A)') 'transform reported errors'
            write (error_unit, '(A)') trim(errors)
            passed = .false.
        end if

        if (index(generated, 'external :: add_one') > 0) then
            write (error_unit, '(A)') &
                'unexpected external declaration for add_one'
            passed = .false.
        end if

        if (index(generated, 'add_one(arr)') == 0) then
            write (error_unit, '(A)') 'missing add_one call in output'
            passed = .false.
        end if
    end function run_use_associated_external_case

end program test_issue_1571_use_associated_external
