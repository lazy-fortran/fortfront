program test_issue_2251_data_implied_do
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success
    character(len=*), parameter :: expected_stmt = &
        'data (coeff(i), coeff(i + 2), i = 1, 2)/1.0d0, 2.0d0, 3.0d0, 4.0d0 /'

    print *, "=== Codegen: DATA implied-do object list preserved ==="

    call read_example('examples/f90/issue_2251_data_implied_do.f90', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    success = allocated(output)
    if (success) then
        if (index(output, trim(expected_stmt)) == 0) success = .false.
    end if
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. success) then
        print *, "FAILED: DATA implied-do object list was not preserved"
        if (allocated(output)) then
            print *, "OUTPUT:"
            print *, trim(output)
        else
            print *, "OUTPUT missing"
        end if
        if (allocated(error_msg)) then
            print *, "ERRORS:"
            print *, trim(error_msg)
        end if
        stop 1
    end if

    print *, "PASSED"


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_issue_2251_data_implied_do
