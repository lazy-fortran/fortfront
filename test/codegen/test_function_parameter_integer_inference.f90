program test_function_parameter_integer_inference
    use frontend, only: transform_lazy_fortran_string
    use iso_fortran_env, only: error_unit
    implicit none

    if (test_square_integer_inference()) then
        write (*, '(A)') 'PASS: integer inference for ambiguous function parameters'
        stop 0
    else
        write (error_unit, '(A)') 'FAIL: integer inference for ambiguous parameters'
        stop 1
    end if

contains

    function test_square_integer_inference() result(passed)
        logical :: passed
        character(len=:), allocatable :: source
        character(len=:), allocatable :: generated
        character(len=:), allocatable :: errors

        source = 'function square(x)' // new_line('a') // &
                 '    result = x * x' // new_line('a') // &
                 '    return result' // new_line('a') // &
                 'end function' // new_line('a') // &
                 'function noise()' // new_line('a') // &
                 '    val = 2.5' // new_line('a') // &
                 '    noise = val' // new_line('a') // &
                 '    return' // new_line('a') // &
                 'end function' // new_line('a') // &
                 'val = 5' // new_line('a') // &
                 'squared = square(val)' // new_line('a') // &
                 'print *, squared' // new_line('a')

        call transform_lazy_fortran_string(source, generated, errors)
        if (.not. allocated(generated)) generated = ''

        passed = .true.
        if (allocated(errors) .and. len_trim(errors) > 0) then
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
            write (error_unit, '(A)') 'missing integer declaration for caller variable'
            passed = .false.
        end if
        if (index(generated, 'real :: x') > 0) then
            write (error_unit, '(A)') 'unexpected real parameter declaration'
            passed = .false.
        end if
    end function test_square_integer_inference

end program test_function_parameter_integer_inference
