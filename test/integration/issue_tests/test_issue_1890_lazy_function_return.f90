program test_issue_1890_lazy_function_return
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    write (error_unit, '(A)') 'INFO: issue_1890 test start'
    flush (error_unit)
    call read_example('examples/lf/issue_1890_lazy_function_return.lf', source)
    if (allocated(source)) then
        write (error_unit, '(A,I0)') 'INFO: example loaded, len=', len(source)
    else
        write (error_unit, '(A)') 'INFO: example loaded, unallocated'
    end if
    flush (error_unit)
    call transform_lazy_fortran_string(source, transformed, error_msg)
    if (allocated(transformed)) then
        write (error_unit, '(A,I0)') 'INFO: transform done, len=', len(transformed)
    else
        write (error_unit, '(A)') 'INFO: transform done, unallocated'
    end if
    flush (error_unit)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error:', trim(error_msg)
            stop 1
        end if
    end if

    if (.not. allocated(transformed)) then
        print *, 'FAIL: transformation produced no output'
        stop 1
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

contains

    include '../../common/read_example.inc'
end program test_issue_1890_lazy_function_return
