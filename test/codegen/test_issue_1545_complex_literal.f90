program test_issue_1545_complex_literal
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Issue #1545: Complex literals with kind parameter ==="

    ! Test complex literals in declaration with kind parameter
    ! This was the main bug: complex(kind=8) was not recognized
    source = '! Issue 1545: complex literal with kind' // new_line('a') // &
             'program test' // new_line('a') // &
             '    implicit none' // new_line('a') // &
             '    complex(kind=8) :: w = (1.0d0, 2.0d0)' // new_line('a') // &
             '    print *, w' // new_line('a') // &
             'end program test'

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.

    if (success) then
        ! Must contain complex literal with both parts
        if (index(output, '(1.0d0, 2.0d0)') == 0) then
            if (index(output, '(1.0d0,2.0d0)') == 0) success = .false.
        end if
        ! Must NOT be just the real part (this was the bug)
        if (index(output, 'w = 1.0d0') > 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: complex(kind=8) literal imaginary part lost'
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

end program test_issue_1545_complex_literal
