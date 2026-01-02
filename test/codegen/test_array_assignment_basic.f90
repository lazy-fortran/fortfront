program test_array_assignment_basic
    ! Regression for issue #869: array assignment should be preserved in codegen
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call read_example('examples/lf/array_element_assignment.lf', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'arr(5) = 100') > 0 .or. index(output, 'arr(5)=100') > 0) then
        print *, 'PASS: array assignment preserved'
    else
        print *, 'FAIL: array assignment missing in output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_array_assignment_basic
