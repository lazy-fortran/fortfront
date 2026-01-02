program test_equivalence_common_block
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/equivalence_common_block.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'allocatable :: equivalence') > 0) then
        print *, 'FAIL: generated allocatable declaration for legacy keyword'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'equivalence(i, r) =') > 0) then
        print *, 'FAIL: generated array assignment for legacy keyword'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: legacy equivalence/common statements handled gracefully'


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_equivalence_common_block
