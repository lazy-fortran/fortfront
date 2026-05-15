program test_issue_1897_typed_array_constructor
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                              iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/issue_1897_typed_array_constructor.f90', source)
    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error:', trim(error_msg)
            stop 1
        end if
    end if

    if (index(transformed, 'real :: arr1(5)') == 0 .and. &
        index(transformed, 'real(dp) :: arr1(5)') == 0) then
        print *, 'FAIL: arr1 declaration lost or incorrect'
        print *, transformed
        stop 1
    end if

    if (index(transformed, 'integer :: arr1') /= 0) then
        print *, 'FAIL: arr1 declaration changed to integer'
        print *, transformed
        stop 1
    end if

    if (index(transformed, 'integer :: arr2(3)') == 0) then
        print *, 'FAIL: arr2 declaration lost or incorrect'
        print *, transformed
        stop 1
    end if

    if (index(transformed, 'real :: arr2') /= 0 .or. &
        index(transformed, 'real(dp) :: arr2') /= 0) then
        print *, 'FAIL: arr2 declaration changed to real'
        print *, transformed
        stop 1
    end if

    if (index(transformed, '(/real ::') == 0 .and. &
        index(transformed, '(/ real ::') == 0) then
        print *, 'FAIL: type spec removed from arr1 constructor'
        print *, transformed
        stop 1
    end if

    if (index(transformed, '(/integer ::') == 0 .and. &
        index(transformed, '(/ integer ::') == 0) then
        print *, 'FAIL: type spec removed from arr2 constructor'
        print *, transformed
        stop 1
    end if

    print *, 'PASS: typed array constructors preserved with correct declarations'


contains


    include '../../common/read_example.inc'
end program test_issue_1897_typed_array_constructor
