program test_issue_1890_lazy_function_return
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    source = 'function quadratic(a, b, c, x)' // new_line('a') // &
             '    quadratic = a*x**2 + b*x + c' // new_line('a') // &
             'end function' // new_line('a') // &
             ' ' // new_line('a') // &
             'result = quadratic(2.0, -3.0, 1.0, 5.0)' // new_line('a') // &
             'print *, ''Result:'', result'

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error:', trim(error_msg)
            stop 1
        end if
    end if

    if (index(transformed, 'integer function quadratic') /= 0) then
        print *, 'FAIL: quadratic inferred as integer function'
        print *, trim(transformed)
        stop 1
    end if

    if (index(transformed, 'real function quadratic') == 0) then
        print *, 'FAIL: quadratic not declared as real function'
        print *, trim(transformed)
        stop 1
    end if

    if (index(transformed, 'real :: result') == 0) then
        print *, 'FAIL: result variable not inferred as real'
        print *, trim(transformed)
        stop 1
    end if

    if (index(transformed, 'real, intent(in) :: a') == 0 .and. &
        index(transformed, 'real :: a') == 0) then
        print *, 'FAIL: parameter a not inferred as real'
        print *, trim(transformed)
        stop 1
    end if

    print *, 'PASS: lazy Fortran infers real return type for quadratic'
end program test_issue_1890_lazy_function_return
