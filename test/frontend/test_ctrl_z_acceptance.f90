program test_ctrl_z_acceptance
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string

    character(len=:), allocatable :: source, output, error_msg

    ! Ctrl-Z (EOF marker on some systems) should not be treated as binary
    ! when it appears in comments or at end of file
    source = 'x = 5' // char(26)

    call transform_lazy_fortran_string(source, output, error_msg)

    if (index(error_msg, 'binary data') > 0) then
        write (error_unit, '(A)') 'FAIL: Ctrl-Z file misclassified as binary'
        write (error_unit, '(A)') trim(error_msg)
        stop 1
    end if

    if (.not. allocated(output)) then
        write (error_unit, '(A)') 'FAIL: no output produced for Ctrl-Z input'
        stop 1
    end if

    if (len_trim(output) == 0) then
        write (error_unit, '(A)') 'FAIL: empty output for Ctrl-Z input'
        stop 1
    end if

    write (error_unit, '(A)') 'PASS: Ctrl-Z accepted'
end program test_ctrl_z_acceptance
