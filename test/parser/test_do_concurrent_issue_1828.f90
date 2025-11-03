program test_do_concurrent_issue_1828
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    print *, "Testing DO CONCURRENT support (Issue #1828)"

    call read_example('examples/f90/do_concurrent_simple.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'do concurrent') == 0) then
        print *, 'ERROR: DO CONCURRENT missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'end do') == 0 .and. &
        index(output, 'enddo') == 0) then
        print *, 'ERROR: END DO missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'arr(i) = i*2') == 0 .and. &
        index(output, 'arr(i)=i*2') == 0 .and. &
        index(output, 'arr(i) = i * 2') == 0) then
        print *, 'ERROR: loop body missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, '(i = 1:10)') == 0) then
        print *, 'ERROR: DO CONCURRENT range syntax (i = 1:10) missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    call read_example('examples/f90/do_concurrent_multi.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    ! Multi-index DO CONCURRENT currently transforms to nested loops:
    ! outer as regular DO, inner as DO CONCURRENT
    if (index(output, 'do i = 1, 3') == 0 .and. &
        index(output, 'do i=1,3') == 0 .and. &
        index(output, 'do i = 1,3') == 0) then
        print *, 'ERROR: outer DO loop missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'do concurrent') == 0) then
        print *, 'ERROR: inner DO CONCURRENT missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, '(j = 1:3)') == 0) then
        print *, 'ERROR: DO CONCURRENT range syntax for j missing'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'arr(i, j) = i + j') == 0 .and. &
        index(output, 'arr(i,j)=i+j') == 0 .and. &
        index(output, 'arr(i,j) = i + j') == 0) then
        print *, 'ERROR: nested loop body missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: DO CONCURRENT preserved correctly'
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

end program test_do_concurrent_issue_1828
