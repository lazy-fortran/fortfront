program test_array_slice_type_inference
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    implicit none
    character(len=:), allocatable :: source, output, error_msg

    call read_example('examples/lf/issue_array_slice_type_mismatch.lf', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: Transformation failed: ', error_msg
            stop 1
        end if
    end if

    if (index(output, 'integer :: slice(3)') == 0) then
        print *, 'FAIL: Expected integer slice with explicit extent'
        print *, 'Output:'
        print *, output
        stop 1
    end if

    if (index(output, 'allocatable :: slice') > 0) then
        print *, 'FAIL: Slice incorrectly marked allocatable'
        print *, 'Output:'
        print *, output
        stop 1
    end if

    if (index(output, 'real :: slice') > 0 .or. &
        index(output, 'real(8) :: slice') > 0) then
        print *, 'FAIL: Found incorrect real type for slice'
        print *, 'Output:'
        print *, output
        stop 1
    end if

    print *, 'PASS: Array slice type inference correct'


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_array_slice_type_inference
