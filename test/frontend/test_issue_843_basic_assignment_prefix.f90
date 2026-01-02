program test_issue_843_basic_assignment_prefix
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use frontend_transformation, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    logical :: ok

    print *, '=== Issue #843: Preserve assignment after non-Fortran prefix ==='

    ! Reproduce: input with a non-Fortran prefix before an assignment
    call read_example('examples/lf/assignment_with_prefix.lf', input)

    call transform_lazy_fortran_string(input, output, error_msg)

    ok = .true.

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: unexpected diagnostic: ', trim(error_msg)
            ok = .false.
        end if
    end if

    if (.not. allocated(output)) then
        print *, 'ERROR: no output generated'
        ok = .false.
    else
        if (index(output, 'x = 42') == 0) then
            print *, 'ERROR: assignment missing from output'
            print *, trim(output)
            ok = .false.
        end if
        ! Ensure stray prefix does not leak into output as a statement
        if (index(output, new_line('a') // '    Simple') > 0) then
            print *, 'ERROR: stray prefix leaked into output'
            print *, trim(output)
            ok = .false.
        end if
    end if

    if (ok) then
        print *, 'PASS: assignment preserved; prefix ignored'
    else
        stop 1
    end if


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_issue_843_basic_assignment_prefix
