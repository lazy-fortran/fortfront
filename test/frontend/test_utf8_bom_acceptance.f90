program test_utf8_bom_acceptance
    use, intrinsic :: iso_fortran_env, only: error_unit
    use transformation_api, only: transform_lazy_fortran_string

    character(len=:), allocatable :: source, output, error_msg
    character(len=3), parameter :: UTF8_BOM = char(239) // char(187) // char(191)

    ! UTF-8 BOM followed by simple Fortran code should not be treated as binary
    source = UTF8_BOM // 'x = 5'

    call transform_lazy_fortran_string(source, output, error_msg)

    if (index(error_msg, 'binary data') > 0) then
        write (error_unit, '(A)') 'FAIL: UTF-8 BOM file misclassified as binary'
        write (error_unit, '(A)') trim(error_msg)
        stop 1
    end if

    if (.not. allocated(output)) then
        write (error_unit, '(A)') 'FAIL: no output produced for UTF-8 BOM input'
        stop 1
    end if

    if (len_trim(output) == 0) then
        write (error_unit, '(A)') 'FAIL: empty output for UTF-8 BOM input'
        stop 1
    end if

    write (error_unit, '(A)') 'PASS: UTF-8 BOM accepted'
end program test_utf8_bom_acceptance
