program test_issue_1406_namelist
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: preserve NAMELIST declarations ==="

    source = '! ensure namelist declarations survive' // new_line('a') // &
             'real :: temperature' // new_line('a') // &
             'namelist /weather/ temperature' // new_line('a') // &
             'temperature = 285.5' // new_line('a') // &
             'print *, temperature' // new_line('a')

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.

    if (success) then
        if (index(output, 'namelist /weather/ temperature') == 0) success = .false.
        if (index(output, 'real(8) :: temperature') == 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: namelist declaration not preserved'
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

end program test_issue_1406_namelist
