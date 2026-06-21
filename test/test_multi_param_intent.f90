program test_multi_param_intent
    ! Test multi-parameter declarations with intent attributes
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source_code
    character(len=:), allocatable :: result
    character(len=:), allocatable :: error_msg
    logical :: test_passed
    logical :: has_real_inputs
    logical :: has_flag_param
    logical :: has_output_buffer

    print *, "=== Testing Multi-Parameter Declarations with Intent ==="

    ! Test 1: Multiple parameters with same type and intent
    source_code = &
        "module test_mod"//new_line('A')// &
        "contains"//new_line('A')// &
        "    function calc(a, b, c) result(sum)"//new_line('A')// &
        "        integer, intent(in) :: a, b, c"//new_line('A')// &
        "        integer :: sum"//new_line('A')// &
        "        sum = a + b + c"//new_line('A')// &
        "    end function calc"//new_line('A')// &
        "end module test_mod"

    call transform_lazy_fortran_string(source_code, result, error_msg)

    test_passed = index(result, "integer, intent(in) :: a, b, c") > 0 .or. &
                  index(result, "integer :: a, b, c") > 0
    if (test_passed) then
        print *, "  PASS: Multiple parameters with intent(in)"
    else
        print *, "  FAIL: Multiple parameters with intent(in)"
        print *, "Output:"
        print *, trim(result)
    end if

    ! Test 2: Mixed parameter declarations
    source_code = &
        "module math_ops"//new_line('A')// &
        "contains"//new_line('A')// &
        "    subroutine process(x, y, flag, output)"//new_line('A')// &
        "        real, intent(in) :: x, y"//new_line('A')// &
        "        logical, intent(in) :: flag"//new_line('A')// &
        "        real, intent(out) :: output"//new_line('A')// &
        "        if (flag) then"//new_line('A')// &
        "            output = x + y"//new_line('A')// &
        "        else"//new_line('A')// &
        "            output = x - y"//new_line('A')// &
        "        end if"//new_line('A')// &
        "    end subroutine process"//new_line('A')// &
        "end module math_ops"

    call transform_lazy_fortran_string(source_code, result, error_msg)

    has_real_inputs = index(result, "real(dp), intent(in) :: x, y") > 0 .or. &
                      index(result, "real(8), intent(in) :: x, y") > 0 .or. &
                      index(result, "real, intent(in) :: x, y") > 0 .or. &
                      index(result, "real(dp) :: x, y") > 0 .or. &
                      index(result, "real(8) :: x, y") > 0 .or. &
                      index(result, "real :: x, y") > 0

    has_flag_param = index(result, "logical, intent(in) :: flag") > 0 .or. &
                     index(result, "logical :: flag") > 0

    has_output_buffer = index(result, "real(dp), intent(out) :: output") > 0 .or. &
                        index(result, "real(8), intent(out) :: output") > 0 .or. &
                        index(result, "real, intent(out) :: output") > 0

    test_passed = has_real_inputs .and. has_flag_param .and. has_output_buffer

    if (test_passed) then
        print *, "  PASS: Mixed parameter declarations with different intents"
    else
        print *, "  FAIL: Mixed parameter declarations with different intents"
        print *, "Output:"
        print *, trim(result)
    end if

    ! Test 3: Parameters with kind specifiers
    source_code = &
        "module precision_mod"//new_line('A')// &
        "contains"//new_line('A')// &
        "    function dot_product(a, b, c, d) result(res)"//new_line('A')// &
        "        real(8), intent(in) :: a, b, c, d"//new_line('A')// &
        "        real(8) :: res"//new_line('A')// &
        "        res = a*c + b*d"//new_line('A')// &
        "    end function dot_product"//new_line('A')// &
        "end module precision_mod"

    call transform_lazy_fortran_string(source_code, result, error_msg)

    test_passed = index(result, "real(dp) :: a, b, c, d") > 0 .or. &
                  index(result, "real(8) :: a, b, c, d") > 0 .or. &
                  index(result, "real :: a, b, c, d") > 0 .or. &
                  index(result, "real(dp), intent(in) :: a, b, c, d") > 0 .or. &
                  index(result, "real(8), intent(in) :: a, b, c, d") > 0 .or. &
                  index(result, "real, intent(in) :: a, b, c, d") > 0

    if (test_passed) then
        print *, "  PASS: Parameters with kind specifiers"
    else
        print *, "  FAIL: Parameters with kind specifiers"
        print *, "Output:"
        print *, trim(result)
    end if

    ! Test 4: Optional parameters
    source_code = &
        "module optional_mod"//new_line('A')// &
        "contains"//new_line('A')// &
        "    subroutine config(a, b, c)"//new_line('A')// &
        "        integer, intent(in) :: a"//new_line('A')// &
        "        integer, intent(in), optional :: b, c"//new_line('A')// &
        "        if (present(b)) then"//new_line('A')// &
        "            print *, a + b"//new_line('A')// &
        "        end if"//new_line('A')// &
        "    end subroutine config"//new_line('A')// &
        "end module optional_mod"

    call transform_lazy_fortran_string(source_code, result, error_msg)

    test_passed = (index(result, "integer, intent(in) :: a") > 0 .or. &
                   index(result, "integer :: a") > 0) .and. &
                  (index(result, "integer, intent(in), optional :: b, c") > 0 .or. &
                   index(result, "integer, optional :: b, c") > 0 .or. &
                   index(result, "integer :: b, c") > 0)

    if (test_passed) then
        print *, "  PASS: Optional parameters"
    else
        print *, "  FAIL: Optional parameters"
        print *, "Output:"
        print *, trim(result)
    end if

    print *, ""
    print *, "All multi-parameter intent tests completed!"

end program test_multi_param_intent
