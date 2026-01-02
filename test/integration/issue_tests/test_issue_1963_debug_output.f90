program test_issue_1963_debug_output
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    all_passed = ensure_debug_absent()

    if (all_passed) then
        print *, 'PASS: Issue #1963 - debug output suppressed'
    else
        error stop 'FAIL: Issue #1963 regression detected'
    end if

contains

    include '../../common/cli_io_reader.inc'
    include '../../common/read_example.inc'


    logical function ensure_debug_absent()
        character(len=:), allocatable :: source
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg
        integer :: pos
        integer :: end_pos

        ensure_debug_absent = .true.

        call read_example('examples/lf/issue_1963_debug_output.lf', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                write (error_unit, '(A)') &
                    'FAIL: unexpected error: ' // trim(error_msg)
                ensure_debug_absent = .false.
                return
            end if
        end if

        if (.not. allocated(output)) then
            write (error_unit, '(A)') 'FAIL: no output captured'
            ensure_debug_absent = .false.
            return
        end if

        pos = index(output, 'DEBUG ')
        if (pos > 0) then
            end_pos = min(len(output), pos + 79)
            write (error_unit, '(A)') 'FAIL: debug text leaked into output'
            write (error_unit, '(A)') output(pos:end_pos)
            ensure_debug_absent = .false.
        end if

        if (index(output, 'end program main') == 0) then
            write (error_unit, '(A)') 'FAIL: expected program terminator missing'
            ensure_debug_absent = .false.
        end if
    end function ensure_debug_absent

end program test_issue_1963_debug_output
