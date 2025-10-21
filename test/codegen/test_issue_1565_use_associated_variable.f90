program test_issue_1565_use_associated_variable
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Issue #1565: use-associated variable not redeclared ==="

    source = 'module test_module' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    integer :: module_var' // new_line('a') // &
             'contains' // new_line('a') // &
             '    subroutine module_sub()' // new_line('a') // &
             '        print *, "Module subroutine"' // new_line('a') // &
             '    end subroutine module_sub' // new_line('a') // &
             'end module test_module' // new_line('a') // &
             '' // new_line('a') // &
             'program main' // new_line('a') // &
             '    use test_module' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    module_var = 42' // new_line('a') // &
             '    call module_sub()' // new_line('a') // &
             'end program main'

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.

    if (success) then
        if (index(output, 'use test_module') == 0) success = .false.
        if (index(output, 'module_var = 42') == 0) success = .false.
        if (index(output, 'call module_sub()') == 0) success = .false.
        if (index(output, 'program main') > 0) then
            if (index(output(index(output, 'program main'):), &
                      'integer :: module_var') > 0) then
                success = .false.
            end if
        end if
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: use-associated variable incorrectly redeclared'
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

end program test_issue_1565_use_associated_variable
