program test_issue_1969_case_insensitive_variables
    use transformation_api, only: transform_lazy_fortran_string
    use string_utils_mod, only: to_lower
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    character(:), allocatable :: lowered
    integer :: result_decls
    integer :: x_decls
    integer :: y_decls

    print *, '=== Issue #1969: Case insensitive variable handling ==='

    input_code = 'X = 5' // new_line('a') // &
                 'y = 10' // new_line('a') // &
                 'Result = x + Y' // new_line('a') // &
                 'print *, ''Result:'', result'

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, 'FAIL: transformation reported an error'
        print *, trim(error_msg)
        error stop 1
    end if

    lowered = to_lower(output_code)

    result_decls = count_occurrences(lowered, ':: result')
    if (result_decls /= 1) then
        print *, 'FAIL: result declared incorrect number of times'
        print *, trim(output_code)
        error stop 1
    end if

    x_decls = count_occurrences(lowered, ':: x')
    if (x_decls /= 1) then
        print *, 'FAIL: x declared incorrect number of times'
        print *, trim(output_code)
        error stop 1
    end if

    y_decls = count_occurrences(lowered, ':: y')
    if (y_decls == 0) then
        if (index(lowered, ', y') == 0) then
            print *, 'FAIL: y declaration missing'
            print *, trim(output_code)
            error stop 1
        end if
    else if (y_decls > 1) then
        print *, 'FAIL: y declared more than once'
        print *, trim(output_code)
        error stop 1
    end if

    if (index(lowered, ':: result, result') > 0) then
        print *, 'FAIL: duplicate mixed case declaration detected'
        print *, trim(output_code)
        error stop 1
    end if

    if (index(lowered, 'result = x + y') == 0) then
        print *, 'FAIL: assignment not canonicalized'
        print *, trim(output_code)
        error stop 1
    end if

    print *, 'PASS: mixed case identifiers handled correctly'

contains

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
