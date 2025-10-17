program test_issue_1411_module_main
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: preserve module + top-level statements ==="

    source = '! module followed by lazy main program' // new_line('a') // &
             'module scalar_ops' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    interface double_it' // new_line('a') // &
             '        module procedure double_int' // new_line('a') // &
             '        module procedure double_real' // new_line('a') // &
             '    end interface' // new_line('a') // &
             'contains' // new_line('a') // &
             '    function double_int(x)' // new_line('a') // &
             '        implicit none' // new_line('a') // &
             '        integer, intent(in) :: x' // new_line('a') // &
             '        integer :: double_int' // new_line('a') // &
             '        double_int = 2 * x' // new_line('a') // &
             '    end function double_int' // new_line('a') // &
             '' // new_line('a') // &
             '    function double_real(x)' // new_line('a') // &
             '        implicit none' // new_line('a') // &
             '        real(8), intent(in) :: x' // new_line('a') // &
             '        real(8) :: double_real' // new_line('a') // &
             '        double_real = 2.0d0 * x' // new_line('a') // &
             '    end function double_real' // new_line('a') // &
             'end module scalar_ops' // new_line('a') // &
             '' // new_line('a') // &
             'use scalar_ops' // new_line('a') // &
             'print *, double_it(3)' // new_line('a') // &
             'print *, double_it(1.5d0)'

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.

    if (success) then
        if (index(output, 'module scalar_ops') == 0) success = .false.
        if (index(output, 'program main') == 0) success = .false.
        if (index(output, 'use scalar_ops') == 0) success = .false.
        if (index(output, 'print *, double_it(3)') == 0) success = .false.
        if (index(output, 'print *, double_it(1.5d0)') == 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: module/main round-trip lost statements'
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

end program test_issue_1411_module_main
