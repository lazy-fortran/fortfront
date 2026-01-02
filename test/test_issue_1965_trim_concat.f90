program test_issue_1965_trim_concat
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    use string_utils_mod, only: to_lower
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lowered_output

    call read_example('examples/lf/issue_1965_trim_concat.lf', input_code)
    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation reported an error'
        write (error_unit, '(A)') trim(error_msg)
        error stop 1
    end if

    lowered_output = to_lower(output_code)

    if (index(lowered_output, 'character(len=10) :: full') == 0) then
        write (error_unit, '(A)') 'FAIL: concatenated result not sized correctly'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(lowered_output, 'full = trim(name) //') == 0) then
        write (error_unit, '(A)') 'FAIL: concatenation assignment missing'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    print *, 'PASS: concatenation length inferred correctly'


contains


    include 'common/read_example.inc'
end program test_issue_1965_trim_concat
