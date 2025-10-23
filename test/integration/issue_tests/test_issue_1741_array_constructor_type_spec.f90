! Test array constructor with type specification (issue #1741)
program test_issue_1741_array_constructor_type_spec
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    source = 'program test_array_constructor_type' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    real :: real_arr(3)' // new_line('a') // &
             '' // new_line('a') // &
             '    real_arr = (/ real :: 1, 2, 3 /)' // new_line('a') // &
             '' // new_line('a') // &
             '    print *, real_arr' // new_line('a') // &
             'end program test_array_constructor_type'

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error:', trim(error_msg)
            stop 1
        end if
    end if

    ! The assignment statement must be present
    if (index(transformed, 'real_arr =') == 0) then
        print *, 'FAIL: assignment statement with type-spec array constructor was removed'
        print *, 'Transformed code:'
        print *, transformed
        stop 1
    end if

    ! The array constructor should be preserved (type spec stripped)
    if (index(transformed, '(/') == 0) then
        print *, 'FAIL: array constructor syntax lost'
        print *, 'Transformed code:'
        print *, transformed
        stop 1
    end if

    print *, 'PASS: array constructor with type spec preserved'

end program test_issue_1741_array_constructor_type_spec
