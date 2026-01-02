program test_issue_1578_block_data
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/issue_1578_block_data.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'block data init_data') == 0) then
        print *, 'FAIL: BLOCK DATA construct lost'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'end block data') == 0) then
        print *, 'FAIL: END BLOCK DATA lost'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'common /shared/') == 0 .and. &
        index(output, 'common/shared') == 0) then
        print *, 'FAIL: COMMON block lost'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, '!!') > 0 .or. index(output, '! common') > 0) then
        print *, 'FAIL: COMMON/DATA converted to comments instead of preserved'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'data global_val') == 0) then
        print *, 'FAIL: DATA statement for global_val missing'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'program main') > 0) then
        print *, 'FAIL: Unexpected synthetic program main wrapper emitted'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: BLOCK DATA preserved after explicit program'


contains


    include '../../common/read_example.inc'
end program test_issue_1578_block_data
