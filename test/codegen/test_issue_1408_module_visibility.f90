program test_issue_1408_module_visibility
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: preserve module visibility specifiers ==="

    source = '! ensure private/public visibility survives' // new_line('a') // &
             'module math_ops' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    private' // new_line('a') // &
             '    public :: double_value' // new_line('a') // &
             'contains' // new_line('a') // &
             '    function double_value(x)' // new_line('a') // &
             '        implicit none' // new_line('a') // &
             '        integer, intent(in) :: x' // new_line('a') // &
             '        integer :: double_value' // new_line('a') // &
             '        double_value = 2 * x' // new_line('a') // &
             '    end function double_value' // new_line('a') // &
             'end module math_ops' // new_line('a') // &
             new_line('a') // &
             'use math_ops' // new_line('a') // &
             'print *, double_value(5)' // new_line('a')

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.

    if (success) then
        if (index(output, 'private') == 0) success = .false.
        if (index(output, 'public :: double_value') == 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: module visibility statements were not preserved'
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

end program test_issue_1408_module_visibility
