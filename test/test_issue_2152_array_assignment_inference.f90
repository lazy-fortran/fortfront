program test_issue_2152_array_assignment_inference
    ! Test for issue #2152: Array assignment should infer array type (not scalar)
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    use string_utils_mod, only: to_lower
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lowered_output

    call read_example('examples/lf/issue_playtest5_where_scalar_not_array.lf', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'ERROR: ', trim(error_msg)
            stop 1
        end if
    end if

    lowered_output = to_lower(output)

    ! Check that arr is declared as an array
    if (index(lowered_output, 'integer :: arr(5)') == 0 .and. &
        index(lowered_output, 'integer, dimension(5) :: arr') == 0) then
        write (error_unit, '(A)') 'FAIL: arr not declared as array(5)'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') trim(output)
        stop 1
    end if

    ! Check that result is declared as an array (not scalar)
    if (index(lowered_output, 'integer :: result(5)') == 0 .and. &
        index(lowered_output, 'integer, dimension(5) :: result') == 0) then
        write (error_unit, '(A)') 'FAIL: result not declared as array(5)'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') trim(output)
        stop 1
    end if

    ! Ensure result is not declared as a scalar
    if (index(lowered_output, 'integer :: result') > 0 .and. &
        index(lowered_output, 'result(') == 0 .and. &
        index(lowered_output, 'dimension') == 0) then
        write (error_unit, '(A)') 'FAIL: result incorrectly declared as scalar'
        write (error_unit, '(A)') 'Output:'
        write (error_unit, '(A)') trim(output)
        stop 1
    end if

    print *, 'PASS: Array assignment correctly infers array type'


contains

    include 'common/cli_io_reader.inc'

    include 'common/read_example.inc'
end program test_issue_2152_array_assignment_inference
