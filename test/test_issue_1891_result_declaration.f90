program test_issue_1891_result_declaration
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    logical :: has_result_decl

    print *, "=== Issue #1891: result() declaration preservation ==="

    input_code = &
        "program sample" // new_line('A') // &
        "    implicit none" // new_line('A') // &
        "contains" // new_line('A') // &
        "    pure function circle_area(radius) result(area)" // new_line('A') // &
        "        real, intent(in) :: radius" // new_line('A') // &
        "        real :: area" // new_line('A') // &
        "        real, parameter :: pi = 3.14159265" // new_line('A') // &
        "        area = pi * radius**2" // new_line('A') // &
        "    end function circle_area" // new_line('A') // &
        "end program sample"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transformation returned error", trim(error_msg)
        error stop 1
    end if

    has_result_decl = index(output_code, "real :: area") > 0 .or. &
        index(output_code, "real(dp) :: area") > 0
    if (.not. has_result_decl) then
        print *, "FAIL: result variable declaration missing in output"
        print *, "Output:" // new_line('A') // trim(output_code)
        error stop 1
    end if

    print *, "PASS: result variable declaration preserved"

end program test_issue_1891_result_declaration
