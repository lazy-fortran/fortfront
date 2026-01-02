program test_issue_1969_case_insensitive_variables
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    use string_utils_mod, only: to_lower
    implicit none

    character(len=:), allocatable :: input_code
    character(len=:), allocatable :: output_code
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lowered
    integer :: result_decls
    integer :: x_decls
    integer :: y_decls

    call read_example('examples/lf/issue_1969_case_insensitive_variables.lf', &
                      input_code)
    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation reported an error'
        write (error_unit, '(A)') trim(error_msg)
        error stop 1
    end if

    lowered = to_lower(output_code)

    result_decls = count_occurrences(lowered, ':: result')
    if (result_decls /= 1) then
        write (error_unit, '(A)') 'FAIL: result declared incorrect number of times'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    x_decls = count_occurrences(lowered, ':: x')
    if (x_decls /= 1) then
        write (error_unit, '(A)') 'FAIL: x declared incorrect number of times'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    y_decls = count_occurrences(lowered, ':: y')
    if (y_decls == 0) then
        if (index(lowered, ', y') == 0) then
            write (error_unit, '(A)') 'FAIL: y declaration missing'
            write (error_unit, '(A)') trim(output_code)
            error stop 1
        end if
    else if (y_decls > 1) then
        write (error_unit, '(A)') 'FAIL: y declared more than once'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(lowered, ':: result, result') > 0) then
        write (error_unit, '(A)') 'FAIL: duplicate mixed-case declaration detected'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    if (index(lowered, 'result = x + y') == 0) then
        write (error_unit, '(A)') 'FAIL: assignment not canonicalized'
        write (error_unit, '(A)') trim(output_code)
        error stop 1
    end if

    print *, 'PASS: mixed case identifiers handled correctly'

contains

    include 'common/read_example.inc'


    integer function count_occurrences(buffer, pattern) result(total)
        character(len=*), intent(in) :: buffer
        character(len=*), intent(in) :: pattern
        integer :: start_pos
        integer :: found_pos
        integer :: pattern_len

        total = 0
        pattern_len = len(pattern)
        if (pattern_len <= 0) return

        start_pos = 1
        do
            if (start_pos > len(buffer)) exit
            found_pos = index(buffer(start_pos:), pattern)
            if (found_pos == 0) exit
            total = total + 1
            start_pos = start_pos + found_pos + pattern_len - 1
        end do
    end function count_occurrences

end program test_issue_1969_case_insensitive_variables
