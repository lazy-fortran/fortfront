program test_issue_1902_intrinsic_statement
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: preserve INTRINSIC statements ==="

    call read_example('examples/f90/intrinsic_statement_program.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (.not. allocated(output)) success = .false.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (success) then
        if (index(output, 'intrinsic :: sin, cos') == 0) success = .false.
        if (index(output, 'sin(0.0)') == 0) success = .false.
        if (index(output, 'cos(x)') == 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: intrinsic statement was not preserved'
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
end program test_issue_1902_intrinsic_statement
