program test_issue_1965_trim_concat
    use transformation_api, only: transform_lazy_fortran_string
    use string_utils_mod, only: to_lower
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    character(:), allocatable :: lowered_output

    print *, '=== Issue #1965: concatenation length inference ==='

    input_code = &
        "name = 'Fortran'" // new_line('A') // &
        "version = '90'" // new_line('A') // &
        "full = trim(name) // ' ' // trim(version)" // new_line('A') // &
        "print *, 'Full name:', full"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: transformation reported an error'
        print *, trim(error_msg)
        error stop 1
    end if

    lowered_output = to_lower(output_code)

    if (index(lowered_output, 'character(len=10) :: full') == 0) then
        print *, 'FAIL: full variable not sized for concatenated string'
        print *, 'Output:' // new_line('A') // trim(output_code)
        error stop 1
    end if

    if (index(lowered_output, 'full = trim(name) //') == 0) then
        print *, 'FAIL: concatenation assignment missing from output'
        print *, 'Output:' // new_line('A') // trim(output_code)
        error stop 1
    end if

    print *, 'PASS: concatenation length inferred correctly'

end program test_issue_1965_trim_concat
