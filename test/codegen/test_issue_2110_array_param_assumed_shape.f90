program test_issue_2110_array_param_assumed_shape
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    integer :: arr_idx, assumed_shape_idx

    call read_example('examples/lf/issue_2110_array_param_hardcoded.lf', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: transformation error: ' // &
                trim(error_msg)
            error stop 1
        end if
    end if

    arr_idx = index(output, 'arr')
    if (arr_idx == 0) then
        write (error_unit, '(A)') 'FAIL: missing arr parameter in output'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    assumed_shape_idx = index(output, 'arr(:)')
    if (assumed_shape_idx == 0) then
        write (error_unit, '(A)') 'FAIL: arr parameter not using assumed-shape (:)'
        write (error_unit, '(A)') 'Expected: arr(:), Got hardcoded dimension'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    if (index(output, 'arr, dimension(') > 0 .or. &
        index(output, 'dimension(5), intent(in) :: arr') > 0 .or. &
        index(output, 'dimension(3), intent(in) :: arr') > 0) then
        write (error_unit, '(A)') 'FAIL: found hardcoded dimension in arr parameter'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    print *, 'PASS: array parameters use assumed-shape instead of hardcoded dimensions'


contains


    include '../common/read_example.inc'
end program test_issue_2110_array_param_assumed_shape
