program test_issue_2255_inline_if_comment
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    integer :: if_pos, end_if_pos, call_inside_pos, call_after_end

    source = "program demo" // new_line('a') // &
             "    implicit none" // new_line('a') // &
             "    logical :: flag" // new_line('a') // new_line('a') // &
             "    flag = .true." // new_line('a') // new_line('a') // &
             "    if (flag) &" // new_line('a') // &
             "    ! comment in continuation line" // new_line('a') // &
             "        call say_hi()" // new_line('a') // new_line('a') // &
             "contains" // new_line('a') // new_line('a') // &
             "    subroutine say_hi()" // new_line('a') // &
             "        print *, ""Hello!""" // new_line('a') // &
             "    end subroutine say_hi" // new_line('a') // new_line('a') // &
             "end program demo"

    call transform_lazy_fortran_string(source, output, error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transformation error:", trim(error_msg)
        stop 1
    end if

    if_pos = index(output, "if (flag) then")
    if (if_pos == 0) then
        print *, "FAIL: missing IF block in output"
        stop 1
    end if

    end_if_pos = index(output(if_pos:), "end if")
    if (end_if_pos == 0) then
        print *, "FAIL: missing END IF in output"
        stop 1
    end if
    end_if_pos = if_pos + end_if_pos - 2

    call_inside_pos = index(output(if_pos:end_if_pos), "call say_hi()")
    if (call_inside_pos == 0) then
        print *, "FAIL: call say_hi() not inside IF block"
        stop 1
    end if

    call_after_end = index(output(end_if_pos:), "call say_hi()")
    if (call_after_end > 0) then
        print *, "FAIL: call say_hi() duplicated outside IF block"
        stop 1
    end if

    print *, "PASS: inline IF with comment preserves body placement"
end program test_issue_2255_inline_if_comment
