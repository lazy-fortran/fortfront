program test_issue_1412_use_intrinsic
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: success

    print *, "=== Codegen: preserve USE intrinsic ONLY clause ==="

    call read_example('examples/f90/use_intrinsic_only.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    success = .true.
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) success = .false.
    end if

    if (.not. allocated(output)) success = .false.

    if (success) then
        if (index(output, 'use, intrinsic :: iso_fortran_env, only: int32') == 0) success = .false.
        if (index(output, 'integer(int32) :: value') == 0) success = .false.
        if (index(output, 'value = 123_int32') == 0) success = .false.
    end if

    if (success) then
        print *, 'PASSED'
    else
        print *, 'FAILED: intrinsic USE clause or kind expressions stripped'
        if (allocated(output)) then
            print *, 'OUTPUT:'
            print *, trim(output)
        end if
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, 'ERRORS:'
                print *, trim(error_msg)
            end if
        end if
        stop 1
    end if


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_issue_1412_use_intrinsic
