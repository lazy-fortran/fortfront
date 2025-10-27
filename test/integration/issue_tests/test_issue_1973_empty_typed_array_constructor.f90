program test_issue_1973_empty_typed_array_constructor
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    source = 'a = [integer ::]' // new_line('a') // &
             'print *, ''Empty array size:'', size(a)' // new_line('a')

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') 'FAIL: unexpected error: ' // &
                trim(error_msg)
            stop 1
        end if
    end if

    if (index(transformed, 'integer, allocatable :: a(:)') == 0) then
        write (error_unit, '(a)') 'FAIL: allocatable declaration missing'
        write (error_unit, '(a)') transformed
        stop 1
    end if

    if (index(transformed, 'a = [integer ::]') == 0) then
        write (error_unit, '(a)') 'FAIL: assignment dropped or altered'
        write (error_unit, '(a)') transformed
        stop 1
    end if

    if (index(transformed, 'print *, ''Empty array size:''') == 0) then
        write (error_unit, '(a)') 'FAIL: print statement missing'
        write (error_unit, '(a)') transformed
        stop 1
    end if

    write (error_unit, '(a)') &
        'PASS: empty typed array constructor preserved with declaration'
end program test_issue_1973_empty_typed_array_constructor
