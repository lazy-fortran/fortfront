program test_issue_1545_complex_literal
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Issue #1545: Complex literals with kind parameter ==="

    call read_example('examples/f90/complex_literal_kind.f90', source)

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


contains


    include '../common/read_example.inc'
end program test_issue_1545_complex_literal
