program test_issue_1579_complex_assignment
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Issue #1579: Complex assignment literals ==="

    source = 'program test_complex' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    complex :: z1, z2, result, zsum' // new_line('a') // &
             '    z1 = (3.0, 4.0)' // new_line('a') // &
             '    z2 = (1.0, 2.0)' // new_line('a') // &
             '    result = z1 + z2' // new_line('a') // &
             '    zsum = (3.0, 4.0) + z2' // new_line('a') // &
             'end program test_complex'

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.

    if (success) then
        if (index(output, '(3.0') == 0 .or. index(output, '4.0') == 0) then
            success = .false.
        end if
        if (index(output, '(1.0') == 0 .or. index(output, '2.0') == 0) then
            success = .false.
        end if
        if (index(output, 'z1 = 3.0d0') > 0) then
            success = .false.
        end if
        if (index(output, 'z2 = 1.0d0') > 0) then
            success = .false.
        end if
        ! Accept both (3.0, 4.0) and (3.0d0, 4.0d0) forms
        if (index(output, 'zsum = (3.0') == 0 .or. index(output, '4.0') == 0) then
            success = .false.
        end if
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: complex assignment literal imaginary part lost'
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

end program test_issue_1579_complex_assignment
