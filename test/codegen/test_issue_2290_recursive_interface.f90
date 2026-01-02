program test_issue_2290_recursive_interface
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    print *, "=== Issue #2290: recursive integer interface signatures ==="
    call run_recursive_interface_test()
    print *, "All issue #2290 tests completed"

contains

    include '../common/cli_io_reader.inc'
    include '../common/read_example.inc'

    subroutine run_recursive_interface_test()
        logical :: ok

        call read_example('examples/f90/issue_2290_recursive_interface.f90', source)
        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if

        ok = index(output, "recursive integer function factorial") > 0
        if (.not. ok) then
            print *, "  FAIL: recursive integer signature missing"
            print *, trim(output)
            stop 1
        end if

        ok = index(output, "result(res)") > 0
        if (.not. ok) then
            print *, "  FAIL: result clause missing"
            print *, trim(output)
            stop 1
        end if

        print *, "  PASS: recursive integer interface preserved"
    end subroutine run_recursive_interface_test


end program test_issue_2290_recursive_interface
