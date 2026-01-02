program test_issue_1900_block_data_labels
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/issue_1900_block_data_labels.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, '123 block data init_data') == 0) then
        print *, 'FAIL: BLOCK DATA header label missing'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'data a, b/10, 20 /') == 0) then
        print *, 'FAIL: DATA statement lost'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'data x, y/3.5, 7.2 /') == 0) then
        print *, 'FAIL: Second DATA statement lost'
        print *, trim(output)
        stop 1
    end if

    if (index(output, '123 end block data init_data') == 0) then
        print *, 'FAIL: BLOCK DATA end label missing'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'program main') > 0) then
        print *, 'FAIL: Unexpected synthetic program main wrapper emitted'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: Labeled BLOCK DATA preserved'


contains

    include '../../common/cli_io_reader.inc'

    include '../../common/read_example.inc'
end program test_issue_1900_block_data_labels
