program test_array_slice_type_inference
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
        iostat_end, iostat_eor
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

    if (index(output, 'integer, allocatable :: slice(:)') == 0) then
        print *, 'FAIL: Expected integer allocatable slice'
        print *, 'Output:'
        print *, output
        stop 1
    end if

    if (index(output, 'real, allocatable :: slice(:)') > 0) then
        print *, 'FAIL: Found incorrect real type for slice'
        print *, 'Output:'
        print *, output
        stop 1
    end if

    print *, 'PASS: Array slice type inference correct'

contains

    subroutine read_example(filepath, content)
        character(len=*), intent(in) :: filepath
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., filepath, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(filepath)
            error stop 1
        end if
    end subroutine read_example

    include '../common/cli_io_reader.inc'

end program test_array_slice_type_inference
