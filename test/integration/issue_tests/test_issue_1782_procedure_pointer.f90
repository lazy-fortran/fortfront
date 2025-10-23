program test_issue_1782_procedure_pointer
    use fortfront, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    source = 'program test_function_pointer'//new_line('a')// &
             '    implicit none'//new_line('a')// &
             '    procedure(real_func), pointer :: fptr'//new_line('a')// &
             '    real :: result'//new_line('a')// &
             '    '//new_line('a')// &
             '    fptr => square'//new_line('a')// &
             '    result = fptr(5.0)'//new_line('a')// &
             '    print *, ''Result:'', result'//new_line('a')// &
             '    '//new_line('a')// &
             'contains'//new_line('a')// &
             '    function square(x) result(res)'//new_line('a')// &
             '        real, intent(in) :: x'//new_line('a')// &
             '        real :: res'//new_line('a')// &
             '        res = x * x'//new_line('a')// &
             '    end function square'//new_line('a')// &
             '    '//new_line('a')// &
             '    interface'//new_line('a')// &
             '        function real_func(x) result(res)'//new_line('a')// &
             '            real, intent(in) :: x'//new_line('a')// &
             '            real :: res'//new_line('a')// &
             '        end function real_func'//new_line('a')// &
             '    end interface'//new_line('a')// &
             'end program test_function_pointer'

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error from transform_lazy_fortran_string'
            print *, trim(error_msg)
            error stop 1
        end if
    end if

    if (index(transformed, 'procedure(real_func), pointer :: fptr') == 0) then
        print *, 'FAIL: procedure pointer declaration missing or mangled'
        print *, 'Expected: procedure(real_func), pointer :: fptr'
        print *, 'Transformed code:'
        print *, transformed
        error stop 1
    end if

    if (index(transformed, 'fptr => square') == 0) then
        print *, 'FAIL: procedure pointer assignment missing'
        print *, transformed
        error stop 1
    end if

    print *, 'PASS: procedure pointer declarations preserved'

end program test_issue_1782_procedure_pointer
