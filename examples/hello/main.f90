program hello
    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none
    write (output_unit, '(A)') 'HELLO: start'
    write (output_unit, '(A)') 'HELLO: ok'
end program hello

