program test_utf8_comment_acceptance
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string

    character(len=:), allocatable :: source, output, error_msg

    ! Leading UTF-8 comment should not be treated as binary
    source = '! UTF-8: Café π λ – — “quotes” • ✓'

    call transform_lazy_fortran_string(source, output, error_msg)

    if (index(error_msg, 'binary data') > 0) then
        write (error_unit, '(A)') 'FAIL: UTF-8 text misclassified as binary'
        write (error_unit, '(A)') trim(error_msg)
        stop 1
    end if

    if (.not. allocated(output)) then
        write (error_unit, '(A)') 'FAIL: no output produced for UTF-8 input'
        stop 1
    end if

    if (len_trim(output) == 0) then
        write (error_unit, '(A)') 'FAIL: empty output for UTF-8 input'
        stop 1
    end if

    write (error_unit, '(A)') 'PASS: UTF-8 comment accepted'
end program test_utf8_comment_acceptance

