program test_issue_2075_stop_keyword_collision
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    use lexer_core, only: to_lower
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: lowered
    character(len=:), allocatable :: error_msg

    call read_example('examples/lf/issue_2075_stop_keyword_collision_in_function_param.lf', &
        source)
    call transform_lazy_fortran_string(source, output, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(A)') &
                'FAIL: transformation error for issue 2075'
            write (error_unit, '(A)') trim(error_msg)
            error stop 1
        end if
    end if

    if (.not. allocated(output)) then
        write (error_unit, '(A)') 'FAIL: no output produced for issue 2075'
        error stop 1
    end if

    lowered = to_lower(output)

    if (index(lowered, 'function make_range(start, stop, step)') == 0) then
        write (error_unit, '(A)') 'FAIL: stop parameter missing from signature'
        error stop 1
    end if

    if (index(lowered, 'integer, intent(in) :: stop') == 0) then
        write (error_unit, '(A)') 'FAIL: stop parameter declaration missing'
        error stop 1
    end if

    print *, 'PASS: stop parameter preserved'


contains


    include '../../common/read_example.inc'
end program test_issue_2075_stop_keyword_collision
