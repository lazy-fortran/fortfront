program test_io_string_literals
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input_code, output_code, error_msg

    ! Test OPEN statement with longer file path (this triggers the bug)
    input_code = "open(unit=10, file='/tmp/test_namelist.dat', status='replace')"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    ! Check that the file path string literal is preserved without extra spaces
    if (index(output_code, "file='/tmp/test_namelist.dat'") > 0 .or. &
        index(output_code, "file = '/tmp/test_namelist.dat'") > 0) then
        print *, "PASS"
    else
        print *, "FAIL: String literal corrupted"
        print *, "Output:"
        print *, trim(output_code)
        error stop 1
    end if
end program test_io_string_literals
