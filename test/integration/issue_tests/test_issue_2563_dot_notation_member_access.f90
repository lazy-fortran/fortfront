program test_issue_2563_dot_notation_member_access
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Issue #2563: LFortran dot notation member access ==="

    call read_example('examples/lf/issue_2563_dot_notation_member_access.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            success = .false.
            print *, "ERRORS:"
            print *, trim(error_msg)
        end if
    end if

    if (.not. allocated(output)) then
        success = .false.
        print *, "FAILED: output not allocated"
        stop 1
    end if

    if (success) then
        if (index(output, "p%x") == 0) then
            success = .false.
            print *, "FAILED: expected p%x in output"
        end if
        if (index(output, "p.x") > 0) then
            success = .false.
            print *, "FAILED: dot notation should be standardized to %"
        end if
    end if

    if (success) then
        print *, "PASSED"
    else
        print *, "FAILED"
        if (allocated(output)) then
            print *, "OUTPUT:"
            print *, trim(output)
        end if
        stop 1
    end if

contains

    include '../../common/read_example.inc'
end program test_issue_2563_dot_notation_member_access
