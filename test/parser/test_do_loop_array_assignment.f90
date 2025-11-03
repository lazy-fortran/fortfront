program test_do_loop_array_assignment
    ! Regression test for Issue #1271: ensure do loop bodies handle array element assignments
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    print *, "=== Testing array element assignments in do loop bodies (Issue #1271) ==="

    call read_example('examples/f90/do_loop_array_update.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, '! Unparsed') > 0) then
        print *, 'ERROR: unexpected ! Unparsed placeholder emitted'
        stop 1
    end if

    if (index(output, 'arr(i) = arr(i) + 1') == 0 .and. &
        index(output, 'arr(i)=arr(i)+1') == 0) then
        print *, 'ERROR: array assignment missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: parser keeps array element assignments intact inside do loops'
    stop 0

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

end program test_do_loop_array_assignment
