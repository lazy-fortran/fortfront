program test_issue_1902_intrinsic_statement
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: preserve INTRINSIC statements ==="

    source = 'program sample' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    intrinsic :: sin, cos' // new_line('a') // &
             '    real :: x' // new_line('a') // &
             '    x = sin(0.0)' // new_line('a') // &
             '    print *, cos(x)' // new_line('a') // &
             'end program sample' // new_line('a')

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (.not. allocated(output)) success = .false.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (success) then
        if (index(output, 'intrinsic :: sin, cos') == 0) success = .false.
        if (index(output, 'sin(0.0)') == 0) success = .false.
        if (index(output, 'cos(x)') == 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: intrinsic statement was not preserved'
        if (allocated(output)) then
            print *, 'OUTPUT:'
            print *, trim(output)
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'ERRORS:'
                print *, trim(error_msg)
            end if
        end if
        stop 1
    end if

end program test_issue_1902_intrinsic_statement
