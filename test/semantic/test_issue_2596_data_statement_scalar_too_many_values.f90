program test_issue_2596_data_statement_scalar_too_many_values
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    print *, "=== Semantic: DATA scalar with too many values errors ==="

    call read_example('examples/f90/data_statement_scalar_upgrade.f90', source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (error_msg == "") then
        write (error_unit, '(A)') "FAIL: expected semantic error"
        error stop 1
    end if

    if (index(error_msg, "DATA statement has 2 values") == 0) then
        write (error_unit, '(A)') "FAIL: expected error to mention value count"
        write (error_unit, '(A)') trim(error_msg)
        error stop 1
    end if
    if (index(error_msg, "scalar object values") == 0) then
        write (error_unit, '(A)') "FAIL: expected error to mention target name"
        write (error_unit, '(A)') trim(error_msg)
        error stop 1
    end if

    if (index(output, "integer :: values") == 0) then
        write (error_unit, '(A)') "FAIL: expected scalar declaration preserved"
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if
    if (index(output, "values(2)") > 0) then
        write (error_unit, '(A)') "FAIL: unexpected array upgrade in output"
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    print *, "PASS"


contains

    include '../common/cli_io_reader.inc'

    include '../common/read_example.inc'
end program test_issue_2596_data_statement_scalar_too_many_values
