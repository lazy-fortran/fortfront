program test_binary_input_rejection
    use, intrinsic :: iso_fortran_env, only: error_unit
    use frontend, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    source = char(0)//'ELF header'
    call transform_lazy_fortran_string(source, output, error_msg)

    if (index(error_msg, 'binary data') == 0) then
        write(error_unit, '(A)') 'FAIL: binary payload did not raise expected diagnostic'
        write(error_unit, '(A)') trim(error_msg)
        stop 1
    end if

    if (.not. allocated(output)) then
        write(error_unit, '(A)') 'FAIL: no fallback output produced'
        stop 1
    end if

    if (len_trim(output) == 0) then
        write(error_unit, '(A)') 'FAIL: fallback output is empty'
        stop 1
    end if

    write(error_unit, '(A)') 'PASS: binary input rejected gracefully'
end program test_binary_input_rejection
