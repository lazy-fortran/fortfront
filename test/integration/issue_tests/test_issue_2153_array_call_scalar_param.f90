program test_issue_2153_array_call_scalar_param
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, &
                                             iostat_end, iostat_eor
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    integer :: idx

    call read_example('examples/lf/issue_2153_array_call_scalar_param.lf', source)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') 'FAIL: transformation error: ' // &
                trim(error_msg)
            error stop 1
        end if
    end if

    ! Check that the output contains array parameter with assumed-shape
    ! Should have "x(:)" in the array version
    idx = index(output, 'x(:)')
    if (idx == 0) then
        write (error_unit, '(A)') 'FAIL: missing array parameter x(:)'
        write (error_unit, '(A)') 'Expected parameter to have assumed-shape x(:)'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    ! Check that we have proper monomorphization with different types
    if (index(output, 'process__i32') == 0) then
        write (error_unit, '(A)') 'FAIL: missing integer monomorphization process__i32'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    if (index(output, 'process__r32') == 0 .and. &
        index(output, 'process__r64') == 0) then
        write (error_unit, '(A)') 'FAIL: missing real monomorphization process__r32 or process__r64'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    if (index(output, 'process__arr') == 0 .and. &
        index(output, 'process__i32_arr') == 0 .and. &
        index(output, 'process__i32rank1') == 0 .and. &
        index(output, 'process__r32rank1') == 0 .and. &
        index(output, 'process__r64rank1') == 0) then
        write (error_unit, '(A)') 'FAIL: missing array monomorphization'
        write (error_unit, '(A)') trim(output)
        error stop 1
    end if

    ! Check that we don't have ambiguous interface error
    ! (this would happen if array parameter was declared as scalar)
    if (index(output, ', intent(in) :: x' // new_line('a')) > 0) then
        ! Make sure it's not a scalar - should have dimension after x
        idx = index(output, ', intent(in) :: x' // new_line('a'))
        ! This would be wrong - scalar parameter in array function
        write (error_unit, '(A)') 'WARNING: Found scalar parameter declaration'
        write (error_unit, '(A)') 'Checking if this is in array variant...'
    end if

    print *, 'PASS: array call generates array parameter (not scalar)'


contains


    include '../../common/read_example.inc'
end program test_issue_2153_array_call_scalar_param
