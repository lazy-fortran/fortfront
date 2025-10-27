program test_issue_1963_debug_output
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = .true.
    print *, '=== Issue #1963: Debug output suppressed in generated code ==='

    if (.not. ensure_debug_absent()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'Issue #1963 fixed!'
    else
        print *, 'Issue #1963 regression detected!'
        stop 1
    end if

contains

    logical function ensure_debug_absent()
        implicit none
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        integer :: pos
        integer :: end_pos

        ensure_debug_absent = .true.
        print *, 'Validating that CLI output is free of debug noise...'

        source = 'subroutine increment(x, delta)' // new_line('a') // &
                 '    x = x + delta' // new_line('a') // &
                 'end subroutine increment' // new_line('a') // new_line('a') // &
                 'value = 10' // new_line('a') // &
                 'step = 5' // new_line('a') // &
                 'call increment(value, step)' // new_line('a')

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: unexpected error:', trim(error_msg)
                ensure_debug_absent = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            print *, '  FAIL: no output captured'
            ensure_debug_absent = .false.
            return
        end if

        pos = index(output, 'DEBUG ')
        if (pos > 0) then
            end_pos = min(len(output), pos + 79)
            print *, '  FAIL: debug text leaked into generated code'
            print *, output(pos:end_pos)
            ensure_debug_absent = .false.
        end if

        if (index(output, 'end program main') == 0) then
            print *, '  FAIL: expected program terminator missing'
            ensure_debug_absent = .false.
        end if

        if (ensure_debug_absent) then
            print *, '  PASS: generated output has no debug statements'
        end if
    end function ensure_debug_absent

end program test_issue_1963_debug_output
