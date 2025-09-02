program test_array_assignment_basic
    ! Regression for issue #869: array assignment should be preserved in codegen
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    source = "program demo" // new_line('a') // &
             "  implicit none" // new_line('a') // &
             "  integer :: arr(10)" // new_line('a') // &
             "  arr(5) = 100" // new_line('a') // &
             "end program demo"

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'arr(5) = 100') > 0 .or. index(output, 'arr(5)=100') > 0) then
        print *, 'PASS: array assignment preserved'
    else
        print *, 'FAIL: array assignment missing in output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if
end program test_array_assignment_basic
