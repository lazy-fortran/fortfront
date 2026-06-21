program test_issue_1782_procedure_pointer
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/issue_1782_procedure_pointer.f90', source)

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

contains

    include '../../common/read_example.inc'
end program test_issue_1782_procedure_pointer
