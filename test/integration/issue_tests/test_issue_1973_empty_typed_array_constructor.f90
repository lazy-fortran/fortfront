program test_issue_1973_empty_typed_array_constructor
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    call read_example('examples/lf/issue_1973_empty_typed_array_constructor.lf', &
        source)
    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') 'FAIL: unexpected error: ' // &
                trim(error_msg)
            stop 1
        end if
    end if

    if (index(transformed, 'integer :: a(0)') == 0) then
        write (error_unit, '(a)') 'FAIL: zero-length declaration missing'
        write (error_unit, '(a)') transformed
        stop 1
    end if

    if (index(transformed, 'allocatable :: a(:)') /= 0) then
        write (error_unit, '(a)') 'FAIL: allocatable declaration still present'
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
        'PASS: empty typed array constructor uses explicit zero-length array'


contains


    include '../../common/read_example.inc'
end program test_issue_1973_empty_typed_array_constructor
