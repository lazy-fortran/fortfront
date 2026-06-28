program test_issue_1968_lazy_function
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    logical :: has_loop_var_decl
    logical :: has_function_name_decl
    logical :: has_result_assignment
    logical :: has_return_type

    call read_example('examples/lf/issue_1968_lazy_function_result.lf', &
        input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation returned error'
        write (error_unit, '(A)') trim(error_msg)
        error stop 1
    end if

    has_return_type = index(output_code, 'real function array_sum') > 0 .or. &
        index(output_code, 'real(dp) function array_sum') > 0
    if (.not. has_return_type) then
        write (error_unit, '(A)') 'FAIL: missing explicit return type'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    has_function_name_decl = index(output_code, 'real :: array_sum') > 0 .or. &
        index(output_code, 'real(dp) :: array_sum') > 0
    if (has_function_name_decl) then
        write (error_unit, '(A)') 'FAIL: function name declared as variable'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    has_loop_var_decl = index(output_code, 'integer :: i') > 0
    if (.not. has_loop_var_decl) then
        write (error_unit, '(A)') 'FAIL: missing loop variable declaration'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    has_result_assignment = index(output_code, 'array_sum = total') > 0
    if (.not. has_result_assignment) then
        write (error_unit, '(A)') 'FAIL: missing assignment to function result'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    print *, 'PASS: lazy function result variable generated correctly'


contains


    include '../common/read_example.inc'
end program test_issue_1968_lazy_function
