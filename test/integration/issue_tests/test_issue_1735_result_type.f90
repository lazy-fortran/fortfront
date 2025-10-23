program test_issue_1735_result_type
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    source = 'module issue_1735_module'//new_line('a')// &
             '    implicit none'//new_line('a')// &
             'contains'//new_line('a')// &
             '    integer function square(x) result(result)'//new_line('a')// &
             '        integer :: x'//new_line('a')// &
             '        result = x * x'//new_line('a')// &
             '    end function square'//new_line('a')// &
             '    double precision function cube(x) result(res)'//new_line('a')// &
             '        double precision :: x'//new_line('a')// &
             '        res = x * x * x'//new_line('a')// &
             '    end function cube'//new_line('a')// &
             'end module issue_1735_module'

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error from transform_lazy_fortran_string'
            print *, trim(error_msg)
            error stop 1
        end if
    end if

    if (index(transformed, 'integer function square(x) result(result)') == 0) then
        print *, 'FAIL: missing integer return type for result clause'
        print *, transformed
        error stop 1
    end if

    if (index(transformed, 'double precision function cube(x) result(res)') == 0) then
        print *, 'FAIL: missing double precision return type for result clause'
        print *, transformed
        error stop 1
    end if

    print *, 'PASS: explicit result types preserved'

end program test_issue_1735_result_type
