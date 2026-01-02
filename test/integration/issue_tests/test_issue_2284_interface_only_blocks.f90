program test_issue_2284_interface_only_blocks
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
        iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    integer :: iface_pos, end_iface_pos, sub_pos

    call read_example('examples/f90/interface_only_block.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected diagnostics: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'program main') > 0) then
        print *, 'FAIL: synthetic program main emitted'
        print *, trim(output)
        stop 1
    end if

    iface_pos = index(output, 'interface')
    end_iface_pos = index(output, 'end interface')
    sub_pos = index(output, 'subroutine foo')

    if (iface_pos == 0 .or. end_iface_pos == 0 .or. sub_pos == 0) then
        print *, 'FAIL: interface block content missing'
        print *, trim(output)
        stop 1
    end if

    if (.not. (iface_pos < sub_pos .and. sub_pos < end_iface_pos)) then
        print *, 'FAIL: subroutine foo not retained within interface'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: interface-only program units are preserved'


contains


    include '../../common/read_example.inc'
end program test_issue_2284_interface_only_blocks
