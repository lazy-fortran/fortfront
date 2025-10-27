program test_issue_1968_lazy_function
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    logical :: has_loop_var_decl
    logical :: has_function_name_decl
    logical :: has_result_assignment
    logical :: has_return_type

    print *, "=== Issue #1968: lazy function result variable handling ==="

    input_code = &
        "function array_sum(arr, n)" // new_line('A') // &
        "    total = 0.0" // new_line('A') // &
        "    do i = 1, n" // new_line('A') // &
        "        total = total + arr(i)" // new_line('A') // &
        "    end do" // new_line('A') // &
        "    array_sum = total" // new_line('A') // &
        "end function"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transformation returned error"
        print *, trim(error_msg)
        error stop 1
    end if

    has_return_type = index(output_code, "real function array_sum") > 0
    if (.not. has_return_type) then
        print *, "FAIL: missing explicit return type for array_sum"
        print *, trim(output_code)
        error stop 1
    end if

    has_function_name_decl = index(output_code, "real :: array_sum") > 0
    if (has_function_name_decl) then
        print *, "FAIL: function name declared as local variable"
        print *, trim(output_code)
        error stop 1
    end if

    has_loop_var_decl = index(output_code, "integer :: i") > 0
    if (.not. has_loop_var_decl) then
        print *, "FAIL: missing loop variable declaration for i"
        print *, trim(output_code)
        error stop 1
    end if

    has_result_assignment = index(output_code, "array_sum = total") > 0
    if (.not. has_result_assignment) then
        print *, "FAIL: expected assignment to function result not found"
        print *, trim(output_code)
        error stop 1
    end if

    print *, "PASS: lazy function result variable generated correctly"

end program test_issue_1968_lazy_function
