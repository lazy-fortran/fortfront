program test_issue_1900_block_data_after_program
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/issue_1900_block_data_after_program.f90', &
                      source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'program main') > 0) then
        print *, 'FAIL: block data wrapped inside synthetic program'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'block data init_data') == 0) then
        print *, 'FAIL: block data unit missing'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'data a, b/10, 20 /') == 0) then
        print *, 'FAIL: integer DATA statement missing'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'data x, y/3.5_8, 7.2_8 /') == 0) then
        print *, 'FAIL: real DATA statement missing'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: block data preserved after program unit'

contains


    include '../../common/read_example.inc'
end program test_issue_1900_block_data_after_program
