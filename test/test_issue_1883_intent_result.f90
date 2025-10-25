program test_issue_1883_intent_result
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    logical :: has_intent

    print *, "=== Issue #1883: intent attributes with result clause ==="

    input_code = &
        "program test_intent" // new_line('A') // &
        "    implicit none" // new_line('A') // &
        "contains" // new_line('A') // &
        "    function add(a, b) result(c)" // new_line('A') // &
        "        real, intent(in) :: a, b" // new_line('A') // &
        "        real :: c" // new_line('A') // &
        "        c = a + b" // new_line('A') // &
        "    end function add" // new_line('A') // &
        "end program test_intent"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transformation returned error:", trim(error_msg)
        error stop 1
    end if

    has_intent = index(output_code, "real, intent(in) :: a, b") > 0
    if (.not. has_intent) then
        print *, "FAIL: intent attributes dropped from parameters"
        print *, "Output:"
        print *, trim(output_code)
        error stop 1
    end if

    print *, "PASS: intent attributes preserved in functions with result clause"

end program test_issue_1883_intent_result
