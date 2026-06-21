program test_do_concurrent_preservation_issue_1852
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                                                              iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    print *, "Testing DO CONCURRENT preservation (Issue #1852)"

    call read_example('examples/f90/do_concurrent_real_array.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    if (index(output, 'do concurrent') == 0) then
        print *, 'ERROR: DO CONCURRENT construct not preserved'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, '(i = 1:10)') == 0) then
        print *, 'ERROR: DO CONCURRENT range syntax not preserved'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'do i = 1, 10') > 0 .or. &
        index(output, 'do i=1,10') > 0) then
        print *, 'ERROR: DO CONCURRENT converted to regular DO loop'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    if (index(output, 'end do') == 0) then
        print *, 'ERROR: END DO missing from output'
        print *, 'Output:'
        print *, trim(output)
        stop 1
    end if

    print *, 'PASS: DO CONCURRENT construct correctly preserved'
    stop 0

contains

    include '../common/read_example.inc'
end program test_do_concurrent_preservation_issue_1852
