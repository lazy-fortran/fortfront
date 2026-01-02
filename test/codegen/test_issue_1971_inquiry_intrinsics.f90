program test_issue_1971_inquiry_intrinsics
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: inquiry intrinsics return scalar integers ==="

    call read_example('examples/lf/issue_1971_inquiry_intrinsics.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (.not. allocated(output)) success = .false.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (success) then
        if (index(output, 'integer :: lb, n, ub') == 0) success = .false.
        if (index(output, 'allocatable ::') /= 0) success = .false.
        if (index(output, 'size(a)') == 0) success = .false.
        if (index(output, 'lbound(a, 1)') == 0) success = .false.
        if (index(output, 'ubound(a, 1)') == 0) success = .false.
    end if

    if (success) then
        print *, "PASSED"
    else
        print *, "FAILED: inquiry intrinsic inference incorrect"
        if (allocated(output)) then
            print *, "OUTPUT:"
            print *, trim(output)
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "ERRORS:"
                print *, trim(error_msg)
            end if
        end if
        stop 1
    end if


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_issue_1971_inquiry_intrinsics
