program test_issue_2255_inline_if_comment
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        & iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    integer :: if_pos, end_if_pos, call_inside_pos, call_after_end

    call read_example('examples/f90/issue_2255_inline_if_comment.f90', source)

    call transform_lazy_fortran_string(source, output, error_msg)
    if (len_trim(error_msg) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation error: ' // &
            trim(error_msg)
        error stop 1
    end if

    ! The standardizer may keep this as a block or emit the equivalent
    ! single-line IF. Both forms preserve the source-level control flow.
    if_pos = index(output, 'if (flag)')
    if (if_pos == 0) then
        write (error_unit, '(A)') 'FAIL: missing IF in output'
        error stop 1
    end if

    end_if_pos = index(output(if_pos:), 'end if')
    if (index(output(if_pos:), 'if (flag) then') > 0) then
        if (end_if_pos == 0) then
            write (error_unit, '(A)') 'FAIL: missing END IF in output'
            error stop 1
        end if
        end_if_pos = if_pos + end_if_pos - 2
    else
        if (index(output(if_pos:), 'if (flag) call say_hi()') == 0) then
            write (error_unit, '(A)') 'FAIL: malformed single-line IF in output'
            error stop 1
        end if
        end_if_pos = len(output)
    end if

    call_inside_pos = index(output(if_pos:end_if_pos), 'call say_hi()')
    if (call_inside_pos == 0) then
        write (error_unit, '(A)') 'FAIL: call say_hi() not inside IF block'
        error stop 1
    end if

    call_after_end = index(output(end_if_pos:), 'call say_hi()')
    if (call_after_end > 0) then
        write (error_unit, '(A)') 'FAIL: call say_hi() duplicated outside IF'
        error stop 1
    end if

    print *, 'PASS: inline IF with comment preserves body placement'


contains


    include 'common/read_example.inc'
end program test_issue_2255_inline_if_comment
