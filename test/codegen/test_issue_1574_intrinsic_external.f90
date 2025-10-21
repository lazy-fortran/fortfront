program test_issue_1574_intrinsic_external
    use frontend, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
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

    function run_intrinsic_external_case() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        source = 'program test_intrinsics' // new_line('a') // &
                 '    implicit none' // new_line('a') // &
                 '    integer :: arr(5)' // new_line('a') // &
                 '    integer :: s, m, n' // new_line('a') // &
                 '' // new_line('a') // &
                 '    arr = [1, 5, 3, 9, 2]' // new_line('a') // &
                 '' // new_line('a') // &
                 '    s = sum(arr)' // new_line('a') // &
                 '    m = maxval(arr)' // new_line('a') // &
                 '    n = size(arr)' // new_line('a') // &
                 '' // new_line('a') // &
                 '    print *, ''Sum:'', s' // new_line('a') // &
                 '    print *, ''Max:'', m' // new_line('a') // &
                 '    print *, ''Size:'', n' // new_line('a') // &
                 'end program test_intrinsics'

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
