program test_issue_2064_logical_return_inferred_as_integer
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use lexer_core, only: to_lower
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lower_output

    call read_example('examples/lf/issue_2064_logical_return_inferred_as_integer.lf', &
                      source)

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write(error_unit, '(a)') &
                'FAIL: transformation reported error for issue_2064'
            write(error_unit, '(a)') trim(error_msg)
            error stop 1
        end if
    end if

    if (.not. allocated(transformed)) then
        write(error_unit, '(a)') 'FAIL: no output generated for issue_2064 example'
        error stop 1
    end if

    lower_output = to_lower(transformed)

    if (index(lower_output, 'logical function is_in_range') == 0) then
        write(error_unit, '(a)') 'FAIL: logical function declaration missing'
        write(error_unit, '(a)') trim(transformed)
        error stop 1
    end if

    if (index(lower_output, 'integer function is_in_range') /= 0) then
        write(error_unit, '(a)') 'FAIL: integer function declaration still present'
        write(error_unit, '(a)') trim(transformed)
        error stop 1
    end if

    write(*, '(a)') 'PASS: issue_2064 logical return type inferred correctly'


contains


    include '../../common/read_example.inc'
end program test_issue_2064_logical_return_inferred_as_integer
