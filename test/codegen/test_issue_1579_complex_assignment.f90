program test_issue_1579_complex_assignment
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Issue #1579: Complex assignment literals ==="

    call read_example('examples/f90/complex_assignment_literals.f90', source)

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


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_issue_1579_complex_assignment
