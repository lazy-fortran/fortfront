program test_implicit_none_dedup
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    integer :: count, pos, start

    print *, 'Testing no duplicate implicit none in program header...'

    call read_example('examples/f90/implicit_none_single.f90', input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)
    if (len(error_msg) > 0) then
        print *, 'ERROR: transform failed:', trim(error_msg)
        stop 1
    end if

    count = 0
    start = 1
    do
        pos = index(output_code(start:), 'implicit none')
        if (pos == 0) exit
        count = count + 1
        start = start + pos + 13
    end do

    if (count /= 1) then
        print *, 'ERROR: expected 1 implicit none, found', count
        print *, trim(output_code)
        stop 1
    end if

    print *, '✓ No duplicate implicit none statements'


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_implicit_none_dedup
