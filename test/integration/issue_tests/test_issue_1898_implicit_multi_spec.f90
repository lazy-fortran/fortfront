program test_issue_1898_implicit_multi_spec
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, result_code, error_msg

    source = "program test_implicit" // new_line('a') // &
             "    implicit real(a-h), integer(i-n)" // new_line('a') // &
             "    x = 3.5" // new_line('a') // &
             "    i = 10" // new_line('a') // &
             "    print *, x, i" // new_line('a') // &
             "end program test_implicit"

    call transform_lazy_fortran_string(source, result_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "ERROR: transform returned:", trim(error_msg)
        error stop "FAIL: unexpected transform error"
    end if

    if (index(result_code, "implicit real (a-h)") == 0) then
        print *, "ERROR: missing implicit real statement"
        print *, result_code
        error stop "FAIL: implicit real missing"
    end if

    if (index(result_code, "implicit integer (i-n)") == 0) then
        print *, "ERROR: missing implicit integer statement"
        print *, result_code
        error stop "FAIL: implicit integer missing"
    end if

    if (index(result_code, "integer(i-n) ::") > 0) then
        print *, "ERROR: invalid declaration detected"
        print *, result_code
        error stop "FAIL: invalid declaration generated"
    end if

    if (index(result_code, "real :: x") == 0 .and. &
        index(result_code, "real(dp) :: x") == 0) then
        print *, "ERROR: missing real :: x declaration"
        print *, result_code
        error stop "FAIL: real declaration missing"
    end if

    if (index(result_code, "integer :: i") == 0) then
        print *, "ERROR: missing integer :: i declaration"
        print *, result_code
        error stop "FAIL: integer declaration missing"
    end if

    print *, "PASS: test_issue_1898_implicit_multi_spec"
end program test_issue_1898_implicit_multi_spec
