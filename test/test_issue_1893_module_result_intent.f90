program test_issue_1893_module_result_intent
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    logical :: has_intent

    print *, "=== Issue #1893: module result() intent preservation ==="

    input_code = &
        "module test_mod" // new_line('A') // &
        "    implicit none" // new_line('A') // &
        "contains" // new_line('A') // &
        "    function add(a, b) result(c)" // new_line('A') // &
        "        real, intent(in) :: a, b" // new_line('A') // &
        "        real :: c" // new_line('A') // &
        "        c = a + b" // new_line('A') // &
        "    end function add" // new_line('A') // &
        "end module test_mod"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transformation returned error:", trim(error_msg)
        error stop 1
    end if

    has_intent = index(output_code, "real, intent(in) :: a, b") > 0
    if (.not. has_intent) then
        print *, "FAIL: intent attributes dropped inside module result() function"
        print *, "Output:"
        print *, trim(output_code)
        error stop 1
    end if

    print *, "PASS: intent attributes preserved for module result() functions"

end program test_issue_1893_module_result_intent
