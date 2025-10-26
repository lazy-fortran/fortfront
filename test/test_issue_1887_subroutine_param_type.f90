program test_issue_1887_subroutine_param_type
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    logical :: has_integer_decl
    logical :: lacks_real_decl

    print *, "=== Issue #1887: infer subroutine parameter types from call site ==="

    input_code = &
        "subroutine add_one(x)" // new_line('A') // &
        "    x = x + 1" // new_line('A') // &
        "end subroutine" // new_line('A') // &
        "" // new_line('A') // &
        "a = 5" // new_line('A') // &
        "call add_one(a)" // new_line('A') // &
        "print *, a"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transformation returned error:", trim(error_msg)
        error stop 1
    end if

    has_integer_decl = index(output_code, "integer :: x") > 0
    lacks_real_decl = index(output_code, "real :: x") == 0
    if (.not. (has_integer_decl .and. lacks_real_decl)) then
        print *, "FAIL: parameter type inference did not propagate call-site type"
        print *, "Output:"
        print *, trim(output_code)
        error stop 1
    end if

    print *, "PASS: subroutine parameter type inferred as integer from call site"

end program test_issue_1887_subroutine_param_type
