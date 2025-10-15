program test_basic_function_inference
    use frontend, only: transform_lazy_fortran_string
    use iso_fortran_env, only: error_unit
    implicit none

    if (run_basic_function_test()) then
        write (*, '(A)') 'PASS: basic function inference generates integer types'
        stop 0
    else
        write (error_unit, '(A)') 'FAIL: basic function inference generates integer types'
        stop 1
    end if

contains

    function run_basic_function_test() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        source = 'function square(x)' // new_line('a') // &
                 '    result = x * x' // new_line('a') // &
                 '    return result' // new_line('a') // &
                 'end function' // new_line('a') // new_line('a') // &
                 'val = 5' // new_line('a') // &
                 'squared = square(val)' // new_line('a') // &
                 'print *, squared'

        call transform_lazy_fortran_string(source, generated, errors)

        if (.not. allocated(generated)) generated = ''
        if (.not. allocated(errors)) errors = ''

        passed = .true.

        if (len_trim(errors) > 0) then
            write (error_unit, '(A)') 'transform reported errors:'
            write (error_unit, '(A)') trim(errors)
            passed = .false.
        end if

        if (index(generated, 'integer function square') == 0) then
            write (error_unit, '(A)') 'missing integer function signature'
            passed = .false.
        end if

        if (index(generated, 'integer :: x') == 0) then
            write (error_unit, '(A)') 'missing integer parameter declaration'
            passed = .false.
        end if

        if (index(generated, 'integer :: val') == 0) then
            write (error_unit, '(A)') 'missing integer caller declaration'
            passed = .false.
        end if

        if (index(generated, 'real function square') > 0) then
            write (error_unit, '(A)') 'unexpected real function signature'
            passed = .false.
        end if

        if (index(generated, 'real :: square') > 0) then
            write (error_unit, '(A)') 'unexpected real variable declaration for square'
            passed = .false.
        end if

        if (index(generated, 'real(8) function square') > 0) then
            write (error_unit, '(A)') 'unexpected real(8) function signature'
            passed = .false.
        end if

        if (index(generated, 'real, external :: square') > 0) then
            write (error_unit, '(A)') 'unexpected real external declaration for square'
            passed = .false.
        end if
    end function run_basic_function_test

end program test_basic_function_inference
