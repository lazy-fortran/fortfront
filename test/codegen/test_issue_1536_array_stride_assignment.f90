program test_issue_1536_array_stride_assignment
    ! Regression test for issue #1536: array section assignment with stride
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    call read_example('examples/lf/array_stride_assignment.lf', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'arr(1:9:2) = 0') > 0 .or. &
        index(output, 'arr(1:9:2)=0') > 0) then
        print *, 'PASS: array stride assignment preserved'
    else
        print *, 'FAIL: array stride assignment missing in output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

contains

    include '../common/cli_io_reader.inc'

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example
end program test_issue_1536_array_stride_assignment
