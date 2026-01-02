program test_issue_2067_implied_do_allocatable
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use lexer_core, only: to_lower
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg
    character(len=:), allocatable :: lowered

    call read_example('examples/lf/issue_2067_implied_do_array_return_rank_mismatch.lf', &
                      source)

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            write (error_unit, '(a)') &
                'FAIL: transformation reported error for issue_2067 example'
            write (error_unit, '(a)') trim(error_msg)
            error stop 1
        end if
    end if

    if (.not. allocated(transformed)) then
        write (error_unit, '(a)') 'FAIL: no output generated for issue_2067 example'
        error stop 1
    end if

    lowered = to_lower(transformed)

    if (index(lowered, 'integer, dimension(:), allocatable :: result_arr') == 0) then
        write (error_unit, '(a)') 'FAIL: result_arr not inferred as allocatable array'
        write (error_unit, '(a)') trim(transformed)
        error stop 1
    end if

    if (index(lowered, 'integer, dimension(1) :: result_arr') > 0) then
        write (error_unit, '(a)') 'FAIL: fixed-size result_arr declaration still present'
        write (error_unit, '(a)') trim(transformed)
        error stop 1
    end if

    write (error_unit, '(a)') 'PASS: issue_2067 allocatable inference works'


contains


    include '../../common/read_example.inc'
end program test_issue_2067_implied_do_allocatable
